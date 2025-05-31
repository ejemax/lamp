;; Enhanced Garage Storage with Value Increment Limits Smart Contract
;; This contract maintains a stored value with advanced features and restrictions

;; Constants
(define-constant MAX_INCREMENT u100) ;; Maximum allowed increment per transaction
(define-constant MIN_DECREMENT u1) ;; Minimum allowed decrement per transaction
(define-constant MAX_DECREMENT u50) ;; Maximum allowed decrement per transaction
(define-constant DAILY_INCREMENT_LIMIT u500) ;; Maximum total increments per day per user
(define-constant CONTRACT_OWNER tx-sender)
(define-constant BLOCKS_PER_DAY u144) ;; Approximately 144 blocks per day (10 min blocks)

;; Error codes
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_INVALID_INCREMENT (err u101))
(define-constant ERR_ZERO_INCREMENT (err u102))
(define-constant ERR_OVERFLOW (err u103))
(define-constant ERR_INVALID_DECREMENT (err u104))
(define-constant ERR_INSUFFICIENT_BALANCE (err u105))
(define-constant ERR_DAILY_LIMIT_EXCEEDED (err u106))
(define-constant ERR_CONTRACT_PAUSED (err u107))
(define-constant ERR_INVALID_MULTIPLIER (err u108))
(define-constant ERR_COOLDOWN_ACTIVE (err u109))

;; Data storage
(define-data-var stored-value uint u0)
(define-data-var total-increments uint u0)
(define-data-var total-decrements uint u0)
(define-data-var contract-paused bool false)
(define-data-var increment-multiplier uint u1) ;; Multiplier for increments (1x by default)
(define-data-var last-update-block uint u0)

;; Maps for tracking user activity
(define-map user-daily-increments 
  { user: principal, day: uint } 
  { amount: uint }
)

(define-map user-last-action 
  principal 
  { block-height: uint, action-type: (string-ascii 10) }
)

(define-map authorized-users 
  principal 
  { authorized: bool, role: (string-ascii 20) }
)

;; Transaction history
(define-map transaction-history 
  uint 
  { 
    user: principal, 
    action: (string-ascii 20), 
    amount: uint, 
    block-height: uint,
    previous-value: uint,
    new-value: uint
  }
)

(define-data-var transaction-counter uint u0)

;; Public functions

;; Enhanced increment with daily limits and multiplier
(define-public (increment-value (increment-amount uint))
  (begin
    ;; Check if contract is paused
    (asserts! (not (var-get contract-paused)) ERR_CONTRACT_PAUSED)
    
    ;; Check if increment amount is valid
    (asserts! (> increment-amount u0) ERR_ZERO_INCREMENT)
    (asserts! (<= increment-amount MAX_INCREMENT) ERR_INVALID_INCREMENT)
    
    ;; Check daily limit
    (let ((current-day (/ block-height BLOCKS_PER_DAY))
          (user-daily-total (get-user-daily-increments tx-sender current-day))
          (multiplied-amount (* increment-amount (var-get increment-multiplier))))
      
      (asserts! (<= (+ user-daily-total multiplied-amount) DAILY_INCREMENT_LIMIT) ERR_DAILY_LIMIT_EXCEEDED)
      
      ;; Get current value and update
      (let ((current-value (var-get stored-value))
            (new-value (+ current-value multiplied-amount)))
        
        ;; Check for overflow
        (asserts! (<= new-value u340282366920938463463374607431768211455) ERR_OVERFLOW)
        
        ;; Update storage
        (var-set stored-value new-value)
        (var-set total-increments (+ (var-get total-increments) multiplied-amount))
        (var-set last-update-block block-height)
        
        ;; Update user daily tracking
        (map-set user-daily-increments 
          { user: tx-sender, day: current-day }
          { amount: (+ user-daily-total multiplied-amount) }
        )
        
        ;; Record transaction
        (record-transaction tx-sender "increment" multiplied-amount current-value new-value)
        
        ;; Update user last action
        (map-set user-last-action tx-sender 
          { block-height: block-height, action-type: "increment" }
        )
        
        (ok new-value)
      )
    )
  )
)

;; Decrement function with limits
(define-public (decrement-value (decrement-amount uint))
  (begin
    ;; Check if contract is paused
    (asserts! (not (var-get contract-paused)) ERR_CONTRACT_PAUSED)
    
    ;; Validate decrement amount
    (asserts! (>= decrement-amount MIN_DECREMENT) ERR_INVALID_DECREMENT)
    (asserts! (<= decrement-amount MAX_DECREMENT) ERR_INVALID_DECREMENT)
    
    (let ((current-value (var-get stored-value)))
      ;; Check sufficient balance
      (asserts! (>= current-value decrement-amount) ERR_INSUFFICIENT_BALANCE)
      
      (let ((new-value (- current-value decrement-amount)))
        ;; Update storage
        (var-set stored-value new-value)
        (var-set total-decrements (+ (var-get total-decrements) decrement-amount))
        (var-set last-update-block block-height)
        
        ;; Record transaction
        (record-transaction tx-sender "decrement" decrement-amount current-value new-value)
        
        ;; Update user last action
        (map-set user-last-action tx-sender 
          { block-height: block-height, action-type: "decrement" }
        )
        
        (ok new-value)
      )
    )
  )
)

;; Batch increment (multiple small increments in one transaction)
(define-public (batch-increment (amounts (list 10 uint)))
  (begin
    (asserts! (not (var-get contract-paused)) ERR_CONTRACT_PAUSED)
    
    (let ((total-amount (fold + amounts u0)))
      (asserts! (<= total-amount MAX_INCREMENT) ERR_INVALID_INCREMENT)
      (increment-value total-amount)
    )
  )
)

;; Set multiplier (owner only)
(define-public (set-increment-multiplier (multiplier uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (and (>= multiplier u1) (<= multiplier u10)) ERR_INVALID_MULTIPLIER)
    (var-set increment-multiplier multiplier)
    (ok multiplier)
  )
)

;; Pause/unpause contract (owner only)
(define-public (toggle-pause)
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (let ((current-state (var-get contract-paused)))
      (var-set contract-paused (not current-state))
      (ok (not current-state))
    )
  )
)

;; Authorize user (owner only)
(define-public (authorize-user (user principal) (role (string-ascii 20)))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (map-set authorized-users user { authorized: true, role: role })
    (ok true)
  )
)

;; Emergency reset (owner only)
(define-public (emergency-reset)
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (var-set stored-value u0)
    (var-set total-increments u0)
    (var-set total-decrements u0)
    (var-set transaction-counter u0)
    (ok u0)
  )
)

;; Set value with history tracking (owner only)
(define-public (set-value (new-value uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (let ((current-value (var-get stored-value)))
      (var-set stored-value new-value)
      (var-set last-update-block block-height)
      (record-transaction tx-sender "set-value" new-value current-value new-value)
      (ok new-value)
    )
  )
)

;; Read-only functions

;; Get current value
(define-read-only (get-current-value)
  (var-get stored-value)
)

;; Get comprehensive stats
(define-read-only (get-contract-stats)
  {
    current-value: (var-get stored-value),
    total-increments: (var-get total-increments),
    total-decrements: (var-get total-decrements),
    net-change: (- (var-get total-increments) (var-get total-decrements)),
    last-update-block: (var-get last-update-block),
    is-paused: (var-get contract-paused),
    current-multiplier: (var-get increment-multiplier),
    transaction-count: (var-get transaction-counter)
  }
)

;; Get user daily increments
(define-read-only (get-user-daily-increments (user principal) (day uint))
  (default-to u0 (get amount (map-get? user-daily-increments { user: user, day: day })))
)

;; Get user's remaining daily limit
(define-read-only (get-user-remaining-daily-limit (user principal))
  (let ((current-day (/ block-height BLOCKS_PER_DAY))
        (used-amount (get-user-daily-increments user current-day)))
    (- DAILY_INCREMENT_LIMIT used-amount)
  )
)

;; Check if user is authorized
(define-read-only (is-user-authorized (user principal))
  (default-to false (get authorized (map-get? authorized-users user)))
)

;; Get user's last action
(define-read-only (get-user-last-action (user principal))
  (map-get? user-last-action user)
)

;; Get transaction history entry
(define-read-only (get-transaction (tx-id uint))
  (map-get? transaction-history tx-id)
)

;; Get all limits and constants
(define-read-only (get-contract-limits)
  {
    max-increment: MAX_INCREMENT,
    min-decrement: MIN_DECREMENT,
    max-decrement: MAX_DECREMENT,
    daily-limit: DAILY_INCREMENT_LIMIT,
    blocks-per-day: BLOCKS_PER_DAY
  }
)

;; Check if increment amount is valid for user
(define-read-only (can-user-increment (user principal) (amount uint))
  (let ((current-day (/ block-height BLOCKS_PER_DAY))
        (user-daily-total (get-user-daily-increments user current-day))
        (multiplied-amount (* amount (var-get increment-multiplier))))
    (and 
      (> amount u0)
      (<= amount MAX_INCREMENT)
      (<= (+ user-daily-total multiplied-amount) DAILY_INCREMENT_LIMIT)
      (not (var-get contract-paused))
    )
  )
)

;; Get value history (last 5 transactions)
(define-read-only (get-recent-transactions)
  (let ((current-counter (var-get transaction-counter)))
    (if (>= current-counter u5)
      {
        tx1: (map-get? transaction-history (- current-counter u4)),
        tx2: (map-get? transaction-history (- current-counter u3)),
        tx3: (map-get? transaction-history (- current-counter u2)),
        tx4: (map-get? transaction-history (- current-counter u1)),
        tx5: (map-get? transaction-history current-counter)
      }
      { tx1: none, tx2: none, tx3: none, tx4: none, tx5: none }
    )
  )
)

;; Private helper functions

;; Record transaction in history
(define-private (record-transaction 
  (user principal) 
  (action (string-ascii 20)) 
  (amount uint) 
  (prev-value uint) 
  (new-value uint))
  (let ((tx-id (+ (var-get transaction-counter) u1)))
    (var-set transaction-counter tx-id)
    (map-set transaction-history tx-id 
      {
        user: user,
        action: action,
        amount: amount,
        block-height: block-height,
        previous-value: prev-value,
        new-value: new-value
      }
    )
    tx-id
  )
)