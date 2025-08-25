;; Delegate Withdrawal Wallet Contract
;; Allows users to delegate withdrawal rights to other addresses with limits

;; Error constants
(define-constant err-owner-only (err u100))
(define-constant err-not-authorized (err u101))
(define-constant err-insufficient-balance (err u102))
(define-constant err-limit-exceeded (err u103))
(define-constant err-invalid-amount (err u104))
(define-constant err-delegate-not-found (err u105))

;; Data variables
(define-data-var contract-owner principal tx-sender)

;; Data maps
;; Map to store user balances
(define-map user-balances principal uint)

;; Map to store delegation information
;; Key: {owner: principal, delegate: principal}
;; Value: {limit: uint, used: uint}
(define-map delegations 
    {owner: principal, delegate: principal}
    {limit: uint, used: uint})

;; Map to track all delegates for an owner (for enumeration)
(define-map owner-delegates principal (list 10 principal))

;; Read-only functions

;; Get user balance
(define-read-only (get-balance (user principal))
    (default-to u0 (map-get? user-balances user)))

;; Get delegation info
(define-read-only (get-delegation (owner principal) (delegate principal))
    (map-get? delegations {owner: owner, delegate: delegate}))

;; Get remaining delegation limit
(define-read-only (get-remaining-limit (owner principal) (delegate principal))
    (match (map-get? delegations {owner: owner, delegate: delegate})
        delegation (- (get limit delegation) (get used delegation))
        u0))

;; Get all delegates for an owner
(define-read-only (get-delegates (owner principal))
    (default-to (list) (map-get? owner-delegates owner)))

;; Check if address is authorized to withdraw
(define-read-only (is-authorized (owner principal) (delegate principal) (amount uint))
    (if (is-eq owner delegate)
        true
        (match (map-get? delegations {owner: owner, delegate: delegate})
            delegation (<= (+ (get used delegation) amount) (get limit delegation))
            false)))

;; Public functions

;; Deposit funds to the contract
(define-public (deposit (amount uint))
    (begin
        (asserts! (> amount u0) err-invalid-amount)
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        (map-set user-balances tx-sender 
            (+ (get-balance tx-sender) amount))
        (ok true)))

;; Add or update a delegate
(define-public (set-delegate (delegate principal) (limit uint))
    (let ((current-delegates (get-delegates tx-sender)))
        (begin
            (asserts! (not (is-eq tx-sender delegate)) err-not-authorized)
            (asserts! (> limit u0) err-invalid-amount)
            
            ;; Add delegate to owner's delegate list if not already present
            (if (is-none (index-of current-delegates delegate))
                (map-set owner-delegates tx-sender 
                    (unwrap! (as-max-len? (append current-delegates delegate) u10) 
                        err-not-authorized))
                true)
            
            ;; Set or update delegation
            (map-set delegations 
                {owner: tx-sender, delegate: delegate}
                {limit: limit, used: u0})
            (ok true))))

;; Remove a delegate
(define-public (remove-delegate (delegate principal))
    (let ((current-delegates (get-delegates tx-sender)))
        (begin
            (asserts! (is-some (index-of current-delegates delegate)) err-delegate-not-found)
            
            ;; Remove from delegations map
            (map-delete delegations {owner: tx-sender, delegate: delegate})
            
            ;; Remove from owner's delegate list
            (map-set owner-delegates tx-sender 
                (filter is-not-delegate current-delegates))
            (ok true))))

;; Helper function for filtering delegates
(define-private (is-not-delegate (potential-delegate principal))
    (not (is-eq potential-delegate tx-sender)))

;; Withdraw funds (can be called by owner or authorized delegate)
(define-public (withdraw (owner principal) (amount uint))
    (let (
        (owner-balance (get-balance owner))
        (delegation-info (get-delegation owner tx-sender))
    )
        (begin
            (asserts! (> amount u0) err-invalid-amount)
            (asserts! (>= owner-balance amount) err-insufficient-balance)
            
            ;; Check authorization
            (if (is-eq owner tx-sender)
                ;; Owner can withdraw any amount up to their balance
                true
                ;; Delegate must be authorized and within limits
                (begin
                    (asserts! (is-some delegation-info) err-not-authorized)
                    (let ((delegation (unwrap-panic delegation-info)))
                        (asserts! (<= (+ (get used delegation) amount) (get limit delegation)) 
                            err-limit-exceeded)
                        ;; Update used amount for delegate
                        (map-set delegations 
                            {owner: owner, delegate: tx-sender}
                            {limit: (get limit delegation), 
                             used: (+ (get used delegation) amount)}))))
            
            ;; Update owner balance
            (map-set user-balances owner (- owner-balance amount))
            
            ;; Transfer STX to the caller
            (try! (as-contract (stx-transfer? amount tx-sender tx-sender)))
            (ok true))))

;; Reset delegate usage (only owner can call this)
(define-public (reset-delegate-usage (delegate principal))
    (match (get-delegation tx-sender delegate)
        delegation 
            (begin
                (map-set delegations 
                    {owner: tx-sender, delegate: delegate}
                    {limit: (get limit delegation), used: u0})
                (ok true))
        err-delegate-not-found))

;; Emergency withdraw (only owner can withdraw all their funds)
(define-public (emergency-withdraw)
    (let ((balance (get-balance tx-sender)))
        (begin
            (asserts! (> balance u0) err-insufficient-balance)
            (map-set user-balances tx-sender u0)
            (try! (as-contract (stx-transfer? balance tx-sender tx-sender)))
            (ok true))))

;; Contract management functions (for contract owner)
(define-public (set-contract-owner (new-owner principal))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) err-owner-only)
        (var-set contract-owner new-owner)
        (ok true)))

(define-read-only (get-contract-owner)
    (var-get contract-owner))