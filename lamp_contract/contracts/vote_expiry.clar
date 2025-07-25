;; Enhanced Voting Contract with Vote Expiry and Advanced Features
;; Comprehensive voting system with multiple election types, delegation, and governance

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-election-not-active (err u101))
(define-constant err-already-voted (err u102))
(define-constant err-invalid-candidate (err u103))
(define-constant err-vote-expired (err u104))
(define-constant err-election-ended (err u105))
(define-constant err-not-eligible (err u106))
(define-constant err-insufficient-tokens (err u107))
(define-constant err-delegate-not-found (err u108))
(define-constant err-self-delegation (err u109))
(define-constant err-proposal-not-found (err u110))
(define-constant err-proposal-ended (err u111))
(define-constant err-invalid-quorum (err u112))
(define-constant err-election-not-found (err u113))
(define-constant err-invalid-election-type (err u114))
(define-constant err-voting-power-zero (err u115))

;; Election Types
(define-constant ELECTION-TYPE-SIMPLE u1)
(define-constant ELECTION-TYPE-WEIGHTED u2)
(define-constant ELECTION-TYPE-DELEGATED u3)
(define-constant ELECTION-TYPE-PROPOSAL u4)
(define-constant ELECTION-TYPE-RANKED u5)

;; Data Variables
(define-data-var current-election-id uint u0)
(define-data-var vote-expiry-period uint u144) ;; Default: ~24 hours
(define-data-var min-voting-power uint u1)
(define-data-var governance-token (optional principal) none)
(define-data-var total-proposals uint u0)
(define-data-var emergency-pause bool false)

;; Enhanced Election Structure
(define-map elections
  { election-id: uint }
  {
    name: (string-ascii 100),
    description: (string-ascii 500),
    election-type: uint,
    active: bool,
    start-block: uint,
    end-block: uint,
    quorum-required: uint,
    created-by: principal,
    total-candidates: uint,
    allow-abstain: bool,
    require-registration: bool
  }
)

;; Enhanced Vote Structure with more metadata
(define-map votes 
  { election-id: uint, voter: principal } 
  { 
    candidate-id: uint, 
    block-height: uint,
    voting-power: uint,
    is-delegated: bool,
    delegator: (optional principal),
    vote-weight: uint,
    ranked-choices: (list 10 uint) ;; For ranked choice voting
  }
)

;; Vote Delegation System
(define-map delegations
  { delegator: principal, election-id: uint }
  {
    delegate: principal,
    voting-power: uint,
    active: bool,
    delegation-block: uint
  }
)

;; Candidate Information with Enhanced Data
(define-map candidates
  { election-id: uint, candidate-id: uint }
  { 
    name: (string-ascii 100),
    description: (string-ascii 500),
    proposal-hash: (optional (buff 32)),
    active: bool,
    campaign-fund: uint,
    endorsements: uint
  }
)

;; Vote Counts per Election and Candidate
(define-map candidate-votes 
  { election-id: uint, candidate-id: uint } 
  { 
    vote-count: uint,
    weighted-votes: uint,
    delegated-votes: uint
  }
)

;; Voter Registration and Eligibility
(define-map voter-registry
  { voter: principal }
  {
    registered: bool,
    registration-block: uint,
    voting-power: uint,
    reputation-score: uint,
    total-votes-cast: uint
  }
)

;; Proposal System for Governance
(define-map proposals
  { proposal-id: uint }
  {
    title: (string-ascii 200),
    description: (string-ascii 1000),
    proposer: principal,
    proposal-type: uint,
    execution-delay: uint,
    min-approval: uint,
    created-block: uint,
    voting-end-block: uint,
    executed: bool,
    votes-for: uint,
    votes-against: uint,
    votes-abstain: uint
  }
)

;; Election Results and Statistics
(define-map election-results
  { election-id: uint }
  {
    winner: (optional uint),
    total-votes: uint,
    turnout-percentage: uint,
    quorum-met: bool,
    finalized: bool,
    finalization-block: uint
  }
)

;; Multi-signature Admin System
(define-map admin-approvals
  { action-hash: (buff 32) }
  {
    approvals: uint,
    required-approvals: uint,
    executed: bool,
    expiry-block: uint
  }
)

;; Voting Power History for Token-based Elections
(define-map voting-power-snapshots
  { voter: principal, block-height: uint }
  { voting-power: uint }
)

;; =============================================================================
;; READ-ONLY FUNCTIONS (Ordered to avoid dependencies)
;; =============================================================================

;; Get voter's base voting power
(define-read-only (get-voter-power (voter principal))
  (match (map-get? voter-registry { voter: voter })
    registry-data (get voting-power registry-data)
    u1 ;; Default power for unregistered voters
  )
)

;; Helper function to calculate delegated power (simplified)
(define-read-only (get-delegated-power (delegate principal) (election-id uint))
  ;; In a real implementation, this would iterate through all delegations
  ;; For now, returning a placeholder value
  u0
)

;; Calculate total voting power including delegations
(define-read-only (get-effective-voting-power (voter principal) (election-id uint))
  (let
    (
      (base-power (get-voter-power voter))
      (delegated-power (get-delegated-power voter election-id))
    )
    (+ base-power delegated-power)
  )
)

;; Enhanced vote validation with delegation support
(define-read-only (is-vote-valid (election-id uint) (voter principal))
  (match (map-get? votes { election-id: election-id, voter: voter })
    vote-data 
    (let 
      (
        (vote-block (get block-height vote-data))
        (election-data (unwrap! (map-get? elections { election-id: election-id }) false))
        (election-start (get start-block election-data))
        (election-end (get end-block election-data))
      )
      ;; Vote is valid if within election period and not expired
      (and 
        (>= vote-block election-start)
        (<= vote-block election-end)
        (<= (- block-height vote-block) (var-get vote-expiry-period))
        (< block-height election-end)
      )
    )
    false
  )
)

;; Get comprehensive election information
(define-read-only (get-election-info (election-id uint))
  (match (map-get? elections { election-id: election-id })
    election-data
    (let
      (
        (results (map-get? election-results { election-id: election-id }))
      )
      (some (merge election-data 
        {
          election-id: election-id,
          results: results,
          current-block: block-height
        }
      ))
    )
    none
  )
)

;; Get voter's delegation status
(define-read-only (get-delegation-info (delegator principal) (election-id uint))
  (map-get? delegations { delegator: delegator, election-id: election-id })
)

;; Get vote information for a voter
(define-read-only (get-vote (election-id uint) (voter principal))
  (map-get? votes { election-id: election-id, voter: voter })
)

;; Get candidate information
(define-read-only (get-candidate (election-id uint) (candidate-id uint))
  (map-get? candidates { election-id: election-id, candidate-id: candidate-id })
)

;; Get vote count for specific candidate (using direct map access)
(define-read-only (get-candidate-vote-count (election-id uint) (candidate-id uint))
  (default-to 
    { vote-count: u0, weighted-votes: u0, delegated-votes: u0 }
    (map-get? candidate-votes { election-id: election-id, candidate-id: candidate-id })
  )
)

;; Get comprehensive vote results for an election
(define-read-only (get-election-results (election-id uint))
  (match (map-get? elections { election-id: election-id })
    election-data
    (let
      (
        (total-candidates (get total-candidates election-data))
        (candidate-1-votes (get-candidate-vote-count election-id u1))
        (candidate-2-votes (get-candidate-vote-count election-id u2))
        (candidate-3-votes (get-candidate-vote-count election-id u3))
        (total-votes (+ (+ (get vote-count candidate-1-votes) (get vote-count candidate-2-votes)) (get vote-count candidate-3-votes)))
        (required-quorum (get quorum-required election-data))
        (total-eligible u100) ;; Simplified - would count all registered voters
        (quorum-met (>= (* total-votes u100) (* total-eligible required-quorum)))
        (results {
          candidate-1: candidate-1-votes,
          candidate-2: candidate-2-votes,
          candidate-3: candidate-3-votes
        })
      )
      (some {
        election-id: election-id,
        results: results,
        quorum-met: quorum-met,
        finalized: (default-to false 
          (get finalized (map-get? election-results { election-id: election-id })))
      })
    )
    none
  )
)

;; Get proposal information
(define-read-only (get-proposal-info (proposal-id uint))
  (map-get? proposals { proposal-id: proposal-id })
)

;; Calculate vote weight based on election type
(define-read-only (calculate-vote-weight (election-data (tuple (name (string-ascii 100)) (description (string-ascii 500)) (election-type uint) (active bool) (start-block uint) (end-block uint) (quorum-required uint) (created-by principal) (total-candidates uint) (allow-abstain bool) (require-registration bool))) (voting-power uint))
  (let
    (
      (election-type (get election-type election-data))
    )
    (if (is-eq election-type ELECTION-TYPE-WEIGHTED)
      voting-power
      u1 ;; Simple voting - each vote counts as 1
    )
  )
)

;; Determine election winner (simplified)
(define-read-only (determine-winner (election-id uint))
  ;; Simplified winner determination - would implement proper logic for different election types
  u1
)

;; Calculate voter turnout percentage
(define-read-only (calculate-turnout (total-votes uint))
  (let
    (
      (total-eligible u100) ;; Simplified
    )
    (if (> total-eligible u0)
      (/ (* total-votes u100) total-eligible)
      u0
    )
  )
)

;; =============================================================================
;; PRIVATE FUNCTIONS
;; =============================================================================

;; Update candidate vote counts
(define-private (update-candidate-votes (election-id uint) (candidate-id uint) (vote-weight uint) (is-delegated bool))
  (let
    (
      (current-votes (get-candidate-vote-count election-id candidate-id))
    )
    (map-set candidate-votes
      { election-id: election-id, candidate-id: candidate-id }
      {
        vote-count: (+ (get vote-count current-votes) u1),
        weighted-votes: (+ (get weighted-votes current-votes) vote-weight),
        delegated-votes: (if is-delegated 
          (+ (get delegated-votes current-votes) vote-weight)
          (get delegated-votes current-votes)
        )
      }
    )
  )
)

;; =============================================================================
;; PUBLIC FUNCTIONS
;; =============================================================================

;; Enhanced election creation with multiple types
(define-public (create-election 
  (name (string-ascii 100))
  (description (string-ascii 500))
  (election-type uint)
  (duration uint)
  (quorum-required uint)
  (allow-abstain bool)
  (require-registration bool))
  (let
    (
      (new-election-id (+ (var-get current-election-id) u1))
    )
    (asserts! (not (var-get emergency-pause)) err-election-not-active)
    (asserts! (<= election-type u5) err-invalid-election-type)
    (asserts! (<= quorum-required u100) err-invalid-quorum)
    
    (map-set elections
      { election-id: new-election-id }
      {
        name: name,
        description: description,
        election-type: election-type,
        active: true,
        start-block: block-height,
        end-block: (+ block-height duration),
        quorum-required: quorum-required,
        created-by: tx-sender,
        total-candidates: u0,
        allow-abstain: allow-abstain,
        require-registration: require-registration
      }
    )
    
    (var-set current-election-id new-election-id)
    (ok new-election-id)
  )
)

;; Register as a voter with enhanced data
(define-public (register-voter (voting-power uint))
  (begin
    (asserts! (not (var-get emergency-pause)) err-election-not-active)
    (asserts! (>= voting-power (var-get min-voting-power)) err-insufficient-tokens)
    
    (map-set voter-registry
      { voter: tx-sender }
      {
        registered: true,
        registration-block: block-height,
        voting-power: voting-power,
        reputation-score: u100, ;; Default reputation
        total-votes-cast: u0
      }
    )
    (ok true)
  )
)

;; Enhanced candidate addition with metadata
(define-public (add-candidate 
  (election-id uint)
  (candidate-id uint) 
  (name (string-ascii 100))
  (description (string-ascii 500))
  (proposal-hash (optional (buff 32))))
  (let
    (
      (election-data (unwrap! (map-get? elections { election-id: election-id }) err-election-not-found))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (get active election-data) err-election-not-active)
    
    (map-set candidates 
      { election-id: election-id, candidate-id: candidate-id }
      { 
        name: name, 
        description: description,
        proposal-hash: proposal-hash,
        active: true,
        campaign-fund: u0,
        endorsements: u0
      }
    )
    
    ;; Initialize vote counts
    (map-set candidate-votes 
      { election-id: election-id, candidate-id: candidate-id } 
      { vote-count: u0, weighted-votes: u0, delegated-votes: u0 }
    )
    
    ;; Update total candidates
    (map-set elections
      { election-id: election-id }
      (merge election-data { total-candidates: (+ (get total-candidates election-data) u1) })
    )
    
    (ok true)
  )
)

;; Enhanced voting with multiple election types support
(define-public (cast-vote 
  (election-id uint) 
  (candidate-id uint)
  (ranked-choices (optional (list 10 uint))))
  (let 
    (
      (election-data (unwrap! (map-get? elections { election-id: election-id }) err-election-not-found))
      (voter-data (map-get? voter-registry { voter: tx-sender }))
      (existing-vote (map-get? votes { election-id: election-id, voter: tx-sender }))
      (voting-power (get-effective-voting-power tx-sender election-id))
    )
    ;; Validate election and voter eligibility
    (asserts! (get active election-data) err-election-not-active)
    (asserts! (< block-height (get end-block election-data)) err-election-ended)
    (asserts! (> voting-power u0) err-voting-power-zero)
    
    ;; Check registration requirement
    (if (get require-registration election-data)
      (asserts! 
        (default-to false (get registered voter-data))
        err-not-eligible
      )
      true
    )
    
    ;; Check if already voted
    (asserts! (is-none existing-vote) err-already-voted)
    
    ;; Validate candidate exists
    (asserts! 
      (is-some (map-get? candidates { election-id: election-id, candidate-id: candidate-id }))
      err-invalid-candidate
    )
    
    ;; Record vote based on election type
    (let
      (
        (vote-weight (calculate-vote-weight election-data voting-power))
        (final-ranked-choices (default-to (list candidate-id) ranked-choices))
      )
      ;; Record the vote
      (map-set votes 
        { election-id: election-id, voter: tx-sender }
        { 
          candidate-id: candidate-id, 
          block-height: block-height,
          voting-power: voting-power,
          is-delegated: false,
          delegator: none,
          vote-weight: vote-weight,
          ranked-choices: final-ranked-choices
        }
      )
      
      ;; Update candidate vote counts
      (update-candidate-votes election-id candidate-id vote-weight false)
      
      ;; Update voter statistics
      (match voter-data
        registry
        (map-set voter-registry
          { voter: tx-sender }
          (merge registry { total-votes-cast: (+ (get total-votes-cast registry) u1) })
        )
        true
      )
      
      (ok true)
    )
  )
)

;; Delegate voting power to another voter
(define-public (delegate-vote (election-id uint) (delegate principal))
  (let
    (
      (election-data (unwrap! (map-get? elections { election-id: election-id }) err-election-not-found))
      (delegator-power (get-voter-power tx-sender))
    )
    (asserts! (not (is-eq tx-sender delegate)) err-self-delegation)
    (asserts! (get active election-data) err-election-not-active)
    (asserts! (< block-height (get end-block election-data)) err-election-ended)
    (asserts! (is-eq (get election-type election-data) ELECTION-TYPE-DELEGATED) err-invalid-election-type)
    
    ;; Check if delegate is registered (if required)
    (if (get require-registration election-data)
      (asserts! 
        (default-to false 
          (get registered (map-get? voter-registry { voter: delegate })))
        err-delegate-not-found
      )
      true
    )
    
    (map-set delegations
      { delegator: tx-sender, election-id: election-id }
      {
        delegate: delegate,
        voting-power: delegator-power,
        active: true,
        delegation-block: block-height
      }
    )
    
    (ok true)
  )
)

;; Create governance proposal
(define-public (create-proposal
  (title (string-ascii 200))
  (description (string-ascii 1000))
  (proposal-type uint)
  (voting-period uint)
  (execution-delay uint)
  (min-approval uint))
  (let
    (
      (new-proposal-id (+ (var-get total-proposals) u1))
    )
    (asserts! (not (var-get emergency-pause)) err-election-not-active)
    
    (map-set proposals
      { proposal-id: new-proposal-id }
      {
        title: title,
        description: description,
        proposer: tx-sender,
        proposal-type: proposal-type,
        execution-delay: execution-delay,
        min-approval: min-approval,
        created-block: block-height,
        voting-end-block: (+ block-height voting-period),
        executed: false,
        votes-for: u0,
        votes-against: u0,
        votes-abstain: u0
      }
    )
    
    (var-set total-proposals new-proposal-id)
    (ok new-proposal-id)
  )
)

;; Vote on governance proposal
(define-public (vote-on-proposal (proposal-id uint) (vote-choice uint) (voting-power uint))
  (let
    (
      (proposal-data (unwrap! (map-get? proposals { proposal-id: proposal-id }) err-proposal-not-found))
    )
    (asserts! (< block-height (get voting-end-block proposal-data)) err-proposal-ended)
    (asserts! (<= vote-choice u2) err-invalid-candidate) ;; 0=against, 1=for, 2=abstain
    
    ;; Update proposal vote counts based on choice
    (let
      (
        (updated-proposal
          (if (is-eq vote-choice u0)
            (merge proposal-data { votes-against: (+ (get votes-against proposal-data) voting-power) })
            (if (is-eq vote-choice u1)
              (merge proposal-data { votes-for: (+ (get votes-for proposal-data) voting-power) })
              (merge proposal-data { votes-abstain: (+ (get votes-abstain proposal-data) voting-power) })
            )
          )
        )
      )
      (map-set proposals { proposal-id: proposal-id } updated-proposal)
      (ok true)
    )
  )
)

;; Finalize election results
(define-public (finalize-election (election-id uint))
  (let
    (
      (election-data (unwrap! (map-get? elections { election-id: election-id }) err-election-not-found))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (>= block-height (get end-block election-data)) err-election-not-active)
    
    ;; Simplified finalization without complex nested matches
    (let
      (
        (winner (some (determine-winner election-id)))
        (total-votes u0) ;; Simplified calculation
        (quorum-met true) ;; Simplified - assume quorum met for now
      )
      (map-set election-results
        { election-id: election-id }
        {
          winner: winner,
          total-votes: total-votes,
          turnout-percentage: (calculate-turnout total-votes),
          quorum-met: quorum-met,
          finalized: true,
          finalization-block: block-height
        }
      )
      
      ;; Mark election as inactive
      (map-set elections
        { election-id: election-id }
        (merge election-data { active: false })
      )
      
      (ok true)
    )
  )
)

;; Emergency pause mechanism
(define-public (set-emergency-pause)
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set emergency-pause true)
    (ok true)
  )
)

;; Resume from emergency pause
(define-public (resume-operations)
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set emergency-pause false)
    (ok true)
  )
)

;; Set governance token for weighted voting
(define-public (set-governance-token (token-contract principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set governance-token (some token-contract))
    (ok true)
  )
)

;; Change vote expiry period (only owner)
(define-public (set-vote-expiry-period (new-period uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set vote-expiry-period new-period)
    (ok true)
  )
)

;; Clean up expired votes (can be called by anyone)
(define-public (cleanup-expired-vote (election-id uint) (voter principal))
  (let 
    (
      (vote-data (unwrap! (map-get? votes { election-id: election-id, voter: voter }) (ok false)))
      (candidate-id (get candidate-id vote-data))
    )
    ;; Check if vote is expired
    (asserts! (not (is-vote-valid election-id voter)) (ok false))
    
    ;; Remove expired vote from candidate count
    (let
      (
        (current-votes (get-candidate-vote-count election-id candidate-id))
      )
      (map-set candidate-votes 
        { election-id: election-id, candidate-id: candidate-id }
        { 
          vote-count: (- (get vote-count current-votes) u1),
          weighted-votes: (- (get weighted-votes current-votes) (get vote-weight vote-data)),
          delegated-votes: (get delegated-votes current-votes)
        }
      )
    )
    
    ;; Remove the expired vote record
    (map-delete votes { election-id: election-id, voter: voter })
    (ok true)
  )
)