# SQL Implementation Plan for Emacs

## Current State Summary
- **Problem**: sqls LSP doesn't handle connection switching well, crashes when switching between localhost/production
- **Goal**: Reliable SQL execution against any database (including production) with secure password handling
- **Existing**: sql-config.el with complex sqls wrapper that doesn't work reliably

## Architecture Decision: Two-System Approach

### 1. sqls for Syntax/Completions (Keep as-is)
- Use localhost-only connections for fast completions
- Configure via `.sqls/config.json` in projects:
  - `~/work/loyal-guru-api/.sqls/config.json` - main project
  - `~/scripts/sql/.sqls/config.json` - symlink to above
- Already working, no changes needed

### 2. pg-el for Query Execution (New)
- Direct PostgreSQL wire protocol implementation
- No persistent connections to avoid password persistence
- Execute queries against any database including production

## Critical Security Finding: pg-el Password Handling

### Current pg-el behavior:
- Stores password in `pgcon-connect-info` (lines 932, 1032, 1057)
- BUT: Password is NEVER read back from connect-info!
- Only used during authentication in `pg-do-startup`
- `pg-cancel` doesn't need password (uses pid/secret instead)

### Required pg-el modifications for lambda support:
```elisp
;; In pg-do-startup, around line 754-758:
;; For AUTH_REQ_CLEARTEXT_PASSWORD (and similar for MD5/SASL)
(let ((actual-password (if (functionp password)
                           (funcall password)
                         password)))
  (pg-send-char con ?p)
  (pg-send-uint con (+ 5 (length actual-password)) 4)
  (pg-send-string con actual-password))

;; In pg-connect functions (lines 932, 1032, 1057):
;; Don't store password at all since it's never used
(setf (pgcon-connect-info con) 
      (list :tcp host port dbname user nil))  ; nil instead of password
```

## Implementation Strategy

### Phase 1: MVP with Wrapper
1. Create `pgx-mode.el` (or similar name) as wrapper around pg-el
2. Function to fetch password from auth-source:
   ```elisp
   (defun pgx-connect (connection-name)
     (let* ((conn-info (assoc connection-name sql-connection-alist))
            (password (my/sql-auth-source-get-password ...)))
       (pg-connect dbname user password host port)))
   ```
3. Execute queries with immediate disconnect (no persistent connections)
4. Display results in buffer

### Phase 2: Upstream Contribution
1. Submit patch to pg-el for lambda password support
2. Minimal change, backwards compatible
3. Improves security by not storing passwords

## File Organization
```
~/.config/emacs/
├── syntax.el              # Add sqls config for sql-mode
├── pgx-mode.el           # New: pg-el wrapper for execution
└── sql-connections.el    # Keep: connection definitions

~/work/loyal-guru-api/.sqls/config.json  # sqls localhost config
~/scripts/sql/.sqls/config.json          # Symlink to above
```

## Connection Definitions (sql-connections.el)
Currently have:
- `localhost` - local PostgreSQL, loyal_guru database
- `production` - 10.49.144.8:5432, loyal_guru database  
- `staging` - 34.76.183.149:10000, staging database

## Package Design Considerations
- Namespace: `pgx-` prefix (PostgreSQL eXtended)
- Dependencies: pg-el, auth-source (built-in)
- Keep functions pure, no global state
- Use `defcustom` for user options
- Clear separation between connection management and execution

## Key Functions to Implement
```elisp
;; Core
(defun pgx-connect-with-auth (connection-name) ...)
(defun pgx-execute-query (connection query) ...)
(defun pgx-disconnect (connection) ...)

;; User commands  
(defun pgx-execute-statement-at-point () ...)
(defun pgx-execute-region (start end) ...)
(defun pgx-execute-buffer () ...)
(defun pgx-switch-connection () ...)  ; Sets buffer-local var

;; Results
(defun pgx-display-results (results) ...)
```

## Security Requirements
1. Never store passwords in memory
2. Use per-query connections (connect → execute → disconnect)
3. Fetch passwords just-in-time from auth-source
4. Clear password variables after use
5. No logging of sensitive data

## Known Issues/Constraints
- psql binary on macOS is `psql-17` (Homebrew naming)
- Production has one schema per client (not replicated locally)
- Must support both localhost and SSL connections
- Rails manages schema, so localhost structure differs from production

## Testing Plan
1. Test localhost connections without auth
2. Test production connections with SSL and auth-source
3. Verify passwords don't persist in memory
4. Test query execution (statement, region, buffer)
5. Verify results display correctly

## Next Steps
1. Delete current sql-config.el.tmpl (broken sqls wrapper)
2. Create pgx-mode.el with basic pg-el wrapper
3. Set up sqls config in syntax.el.tmpl
4. Create .sqls/config.json files
5. Test both systems working together
6. Submit pg-el patch upstream for lambda support