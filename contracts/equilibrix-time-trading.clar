;; EquilibriX - Balanced Time Asset Trading System

;; System operational state
(define-data-var nexus-operational-status bool true)

;; Maximum number of beneficiaries allowed in bulk operations
(define-constant max-beneficiaries u20)

;; Protocol economic parameters
(define-data-var time-unit-tariff uint u500)
(define-data-var artisan-allocation-ceiling uint u100)
(define-data-var marketplace-fee-percentage uint u5)
(define-data-var early-departure-compensation-rate uint u90)
(define-data-var global-time-allocation-ceiling uint u10000)
(define-data-var allocated-time-units uint u0)

;; System administrative definitions
(define-constant contract-steward tx-sender)
(define-constant error-steward-authorization (err u350))
(define-constant error-allocation-shortage (err u351))
(define-constant error-booking-unsuccessful (err u352))
(define-constant error-tariff-invalid (err u353))
(define-constant error-interval-invalid (err u354))
(define-constant error-commission-invalid (err u355))
(define-constant error-compensation-failed (err u356))
(define-constant error-identical-artisan (err u357))
(define-constant error-allocation-ceiling-reached (err u358))
(define-constant error-allocation-ceiling-invalid (err u359))
(define-constant error-already-occupying (err u410))
(define-constant error-no-schedule-found (err u411))
(define-constant error-premature-departure-blocked (err u413))
(define-constant error-nexus-suspended (err u416))
(define-constant error-already-suspended (err u417))
(define-constant error-already-operational (err u418))
(define-constant error-invalid-exchange-registry (err u419))
(define-constant error-exchange-unsuccessful (err u420))
(define-constant error-empty-registry (err u421))
(define-constant error-beneficiary-limit-reached (err u422))

;; Registry structures
(define-map artisan-time-unit-ledger principal uint)
(define-map artisan-credit-ledger principal uint)
(define-map time-units-marketplace {artisan: principal} {intervals: uint, tariff: uint})

;; Activity tracking structures
(define-map active-space-utilization {artisan: principal} {commencement: uint, intervals: uint, status-active: bool})

;; Core calculation functions

;; Calculate the marketplace commission on transactions
(define-private (calculate-marketplace-fee (amount uint))
  (/ (* amount (var-get marketplace-fee-percentage)) u100))

;; Calculate early departure compensation amount
(define-private (calculate-early-departure-compensation (intervals uint))
  (/ (* intervals (var-get time-unit-tariff) (var-get early-departure-compensation-rate)) u100))

;; Update global time allocation counter
(define-private (update-global-time-allocation (intervals int))
  (let (
    (current-allocation (var-get allocated-time-units))
    (new-allocation (if (< intervals 0)
                         (if (>= current-allocation (to-uint (- 0 intervals)))
                             (- current-allocation (to-uint (- 0 intervals)))
                             u0)
                         (+ current-allocation (to-uint intervals))))
  )
    (asserts! (<= new-allocation (var-get global-time-allocation-ceiling)) error-allocation-ceiling-reached)
    (var-set allocated-time-units new-allocation)
    (ok true)))

;; Private function to verify and transfer time units to a single beneficiary
(define-private (transfer-single-allocation (beneficiary principal) (intervals uint))
  (let (
    (sender-balance (default-to u0 (map-get? artisan-time-unit-ledger tx-sender)))
    (beneficiary-balance (default-to u0 (map-get? artisan-time-unit-ledger beneficiary)))
    (updated-beneficiary-balance (+ beneficiary-balance intervals))
  )
    ;; Verify beneficiary is not sender
    (if (is-eq tx-sender beneficiary)
        (err error-identical-artisan)
        ;; Verify intervals > 0
        (if (<= intervals u0)
            (err error-interval-invalid)
            ;; Verify beneficiary won't exceed maximum
            (if (> updated-beneficiary-balance (var-get artisan-allocation-ceiling))
                (err error-allocation-ceiling-reached)
                ;; All checks passed, transfer the intervals
                (begin
                  (map-set artisan-time-unit-ledger beneficiary updated-beneficiary-balance)
                  (ok true)))))))

;; Transaction functions

;; Register time units for marketplace availability
(define-public (register-available-time-units (intervals uint) (tariff uint))
  (let (
    (current-balance (default-to u0 (map-get? artisan-time-unit-ledger tx-sender)))
    (current-registered (get intervals (default-to {intervals: u0, tariff: u0} 
                                        (map-get? time-units-marketplace {artisan: tx-sender}))))
    (new-registered (+ intervals current-registered))
  )
    ;; Validation checks
    (asserts! (> intervals u0) error-interval-invalid)
    (asserts! (> tariff u0) error-tariff-invalid)
    (asserts! (>= current-balance new-registered) error-allocation-shortage)

    ;; Update global allocations and marketplace registry
    (try! (update-global-time-allocation (to-int intervals)))
    (map-set time-units-marketplace {artisan: tx-sender} {intervals: new-registered, tariff: tariff})
    (ok true)))

;; Acquire time units from another artisan through the marketplace
(define-public (acquire-time-units-from-artisan (provider principal) (intervals uint))
  (let (
    (marketplace-data (default-to {intervals: u0, tariff: u0} 
                      (map-get? time-units-marketplace {artisan: provider})))
    (acquisition-cost (* intervals (get tariff marketplace-data)))
    (marketplace-fee (calculate-marketplace-fee acquisition-cost))
    (total-cost (+ acquisition-cost marketplace-fee))
    (provider-allocation (default-to u0 (map-get? artisan-time-unit-ledger provider)))
    (acquirer-credit (default-to u0 (map-get? artisan-credit-ledger tx-sender)))
    (provider-credit (default-to u0 (map-get? artisan-credit-ledger provider)))
    (steward-credit (default-to u0 (map-get? artisan-credit-ledger contract-steward)))
  )
    ;; Validation checks
    (asserts! (not (is-eq tx-sender provider)) error-identical-artisan)
    (asserts! (> intervals u0) error-interval-invalid)
    (asserts! (>= (get intervals marketplace-data) intervals) error-allocation-shortage)
    (asserts! (>= provider-allocation intervals) error-allocation-shortage)
    (asserts! (>= acquirer-credit total-cost) error-allocation-shortage)

    ;; Update provider's allocation and marketplace listing
    (map-set artisan-time-unit-ledger provider (- provider-allocation intervals))
    (map-set time-units-marketplace {artisan: provider} 
             {intervals: (- (get intervals marketplace-data) intervals), 
              tariff: (get tariff marketplace-data)})

    ;; Update acquirer's credit and allocation ledgers
    (map-set artisan-credit-ledger tx-sender (- acquirer-credit total-cost))
    (map-set artisan-time-unit-ledger tx-sender 
             (+ (default-to u0 (map-get? artisan-time-unit-ledger tx-sender)) intervals))

    ;; Process payments to provider and marketplace steward
    (map-set artisan-credit-ledger provider (+ provider-credit acquisition-cost))
    (map-set artisan-credit-ledger contract-steward (+ steward-credit marketplace-fee))

    (ok true)))

;; Process early departure compensation for unused time units
(define-public (request-early-departure-compensation (intervals uint))
  (let (
    (artisan-allocation (default-to u0 (map-get? artisan-time-unit-ledger tx-sender)))
    (compensation-amount (calculate-early-departure-compensation intervals))
    (steward-credit-balance (default-to u0 (map-get? artisan-credit-ledger contract-steward)))
  )
    ;; Validation checks
    (asserts! (> intervals u0) error-interval-invalid)
    (asserts! (>= artisan-allocation intervals) error-allocation-shortage)
    (asserts! (>= steward-credit-balance compensation-amount) error-compensation-failed)

    ;; Update artisan's allocation
    (map-set artisan-time-unit-ledger tx-sender (- artisan-allocation intervals))

    ;; Process compensation to artisan
    (map-set artisan-credit-ledger tx-sender (+ compensation-amount))

    (ok true)))

;; Acquire time units directly from protocol by paying STX
(define-public (purchase-time-unit-allocation (intervals uint))
  (let (
    (cost (* intervals (var-get time-unit-tariff)))
    (current-balance (default-to u0 (map-get? artisan-time-unit-ledger tx-sender)))
    (new-balance (+ current-balance intervals))
    (steward-credit (default-to u0 (map-get? artisan-credit-ledger contract-steward)))
  )
    ;; Validation checks
    (asserts! (> intervals u0) error-interval-invalid)
    (asserts! (<= new-balance (var-get artisan-allocation-ceiling)) error-allocation-ceiling-reached)

    ;; Process payment and allocation updates
    (try! (stx-transfer? cost tx-sender contract-steward))
    (try! (update-global-time-allocation (to-int intervals)))
    (map-set artisan-time-unit-ledger tx-sender new-balance)
    (map-set artisan-credit-ledger contract-steward (+ steward-credit cost))

    (ok true)))

;; Transfer allocated time units between artisans
(define-public (transfer-time-unit-allocation (beneficiary principal) (intervals uint))
  (let (
    (sender-balance (default-to u0 (map-get? artisan-time-unit-ledger tx-sender)))
    (beneficiary-balance (default-to u0 (map-get? artisan-time-unit-ledger beneficiary)))
    (new-beneficiary-balance (+ beneficiary-balance intervals))
  )
    ;; Validation checks
    (asserts! (not (is-eq tx-sender beneficiary)) error-identical-artisan)
    (asserts! (> intervals u0) error-interval-invalid)
    (asserts! (>= sender-balance intervals) error-allocation-shortage)
    (asserts! (<= new-beneficiary-balance (var-get artisan-allocation-ceiling)) error-allocation-ceiling-reached)

    ;; Process the transfer
    (map-set artisan-time-unit-ledger tx-sender (- sender-balance intervals))
    (map-set artisan-time-unit-ledger beneficiary new-beneficiary-balance)

    (ok true)))

;; Administration functions

;; Update protocol parameters
(define-public (update-nexus-parameters (new-tariff (optional uint)) 
                                        (new-fee-percentage (optional uint))
                                        (new-compensation-rate (optional uint))
                                        (new-artisan-ceiling (optional uint))
                                        (new-global-ceiling (optional uint)))
  (begin
    ;; Authorization check
    (asserts! (is-eq tx-sender contract-steward) error-steward-authorization)

    ;; Process tariff update if provided
    (if (is-some new-tariff)
        (let ((tariff (unwrap! new-tariff error-tariff-invalid)))
          (asserts! (> tariff u0) error-tariff-invalid)
          (var-set time-unit-tariff tariff))
        true)

    ;; Process fee percentage update if provided
    (if (is-some new-fee-percentage)
        (let ((percentage (unwrap! new-fee-percentage error-commission-invalid)))
          (asserts! (<= percentage u20) error-commission-invalid) ;; Capped at 20%
          (var-set marketplace-fee-percentage percentage))
        true)

    ;; Process compensation rate update if provided
    (if (is-some new-compensation-rate)
        (let ((rate (unwrap! new-compensation-rate error-commission-invalid)))
          (asserts! (<= rate u100) error-commission-invalid) ;; Capped at 100%
          (var-set early-departure-compensation-rate rate))
        true)

    ;; Process artisan allocation ceiling update if provided
    (if (is-some new-artisan-ceiling)
        (let ((ceiling (unwrap! new-artisan-ceiling error-allocation-ceiling-invalid)))
          (asserts! (> ceiling u0) error-allocation-ceiling-invalid)
          (var-set artisan-allocation-ceiling ceiling))
        true)

    ;; Process global allocation ceiling update if provided
    (if (is-some new-global-ceiling)
        (let ((ceiling (unwrap! new-global-ceiling error-allocation-ceiling-invalid)))
          (asserts! (>= ceiling (var-get allocated-time-units)) error-allocation-ceiling-invalid)
          (var-set global-time-allocation-ceiling ceiling))
        true)

    (ok true)))

;; Withdraw credited funds from protocol
(define-public (withdraw-available-credit (amount uint))
  (let (
    (artisan-credit (default-to u0 (map-get? artisan-credit-ledger tx-sender)))
  )
    ;; Validation checks
    (asserts! (> amount u0) error-tariff-invalid)
    (asserts! (>= artisan-credit amount) error-allocation-shortage)

    ;; Process credit update and STX transfer
    (map-set artisan-credit-ledger tx-sender (- artisan-credit amount))
    (try! (as-contract (stx-transfer? amount tx-sender tx-sender)))

    (ok true)))

;; Cancel registered marketplace time units
(define-public (withdraw-marketplace-registration)
  (let (
    (registered-data (default-to {intervals: u0, tariff: u0} 
                    (map-get? time-units-marketplace {artisan: tx-sender})))
    (registered-intervals (get intervals registered-data))
    (artisan-allocation (default-to u0 (map-get? artisan-time-unit-ledger tx-sender)))
  )
    ;; Validation check
    (asserts! (> registered-intervals u0) error-allocation-shortage)

    ;; Remove the marketplace registration
    (map-delete time-units-marketplace {artisan: tx-sender})

    ;; Update artisan's allocation by reclaiming the intervals
    (map-set artisan-time-unit-ledger tx-sender (+ artisan-allocation registered-intervals))

    (ok true)))

;; Space utilization tracking functions

;; Begin space utilization session
(define-public (commence-space-utilization (intervals uint))
  (let (
    (current-time (unwrap-panic (get-block-info? time u0)))
    (artisan-allocation (default-to u0 (map-get? artisan-time-unit-ledger tx-sender)))
    (active-session (default-to {commencement: u0, intervals: u0, status-active: false} 
                             (map-get? active-space-utilization {artisan: tx-sender})))
  )
    ;; Validation checks
    (asserts! (>= artisan-allocation intervals) error-allocation-shortage)
    (asserts! (not (get status-active active-session)) error-already-occupying)

    ;; Update artisan's allocation balance
    (map-set artisan-time-unit-ledger tx-sender (- artisan-allocation intervals))

    ;; Record session commencement
    (map-set active-space-utilization {artisan: tx-sender} 
             {commencement: current-time, intervals: intervals, status-active: true})

    (ok true)))

;; End space utilization session
(define-public (conclude-space-utilization (reclaim-unused-time bool))
  (let (
    (current-time (unwrap-panic (get-block-info? time u0)))
    (active-session (default-to {commencement: u0, intervals: u0, status-active: false} 
                             (map-get? active-space-utilization {artisan: tx-sender})))
    (start-time (get commencement active-session))
    (reserved-intervals (get intervals active-session))
    (is-active (get status-active active-session))
    (elapsed-seconds (- current-time start-time))
    (elapsed-hours (/ elapsed-seconds u3600)) ;; Convert seconds to hours
    (unused-intervals (if (< elapsed-hours reserved-intervals)
                          (- reserved-intervals elapsed-hours)
                          u0))
  )
    ;; Validation check
    (asserts! is-active error-no-schedule-found)

    ;; Mark session as concluded
    (map-set active-space-utilization {artisan: tx-sender} 
             {commencement: u0, intervals: u0, status-active: false})

    ;; Process unused time reclamation if requested
    (if (and reclaim-unused-time (> unused-intervals u0))
        (let (
            (current-allocation (default-to u0 (map-get? artisan-time-unit-ledger tx-sender)))
        )
          (map-set artisan-time-unit-ledger tx-sender (+ current-allocation unused-intervals))
          (ok unused-intervals))
        (ok u0))
  ))

;; System operational state management

;; Suspend nexus operations
(define-public (suspend-nexus-operations)
  (begin
    ;; Authorization check
    (asserts! (is-eq tx-sender contract-steward) error-steward-authorization)

    ;; Validate current operational state
    (asserts! (not (var-get nexus-operational-status)) error-already-suspended)

    ;; Set nexus to suspended state
    (var-set nexus-operational-status false)

    (ok true)))

;; Resume nexus operations
(define-public (resume-nexus-operations)
  (begin
    ;; Authorization check
    (asserts! (is-eq tx-sender contract-steward) error-steward-authorization)

    ;; Validate current operational state
    (asserts! (var-get nexus-operational-status) error-already-operational)

    ;; Set nexus to operational state
    (var-set nexus-operational-status true)

    (ok true)))


