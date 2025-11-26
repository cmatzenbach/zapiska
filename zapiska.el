;;; zapiska.el --- Russian vocabulary learning system -*- lexical-binding: t -*-
;;
;; Author: Chris Matzenbach
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (hydra "0.15.0") (evil "1.14.0"))
;; Keywords: learning, languages, russian
;; URL: https://github.com/cmatzenbach/zapiska
;;
;;; Commentary:
;;
;; Zapiska is a comprehensive Russian vocabulary learning system for Emacs.
;;
;; Features:
;; - Vocabulary list management (add/edit/delete words)
;; - Quiz mode with spaced repetition (SM-2 algorithm)
;; - Study mode for passive vocabulary review
;; - Visual Cyrillic keyboard reference
;; - Statistics tracking and dashboard
;; - CSV/Anki import/export
;;
;; Usage:
;;   M-x zapiska-open-list    ; Manage vocabulary
;;   M-x zapiska-start-quiz   ; Begin quiz session
;;   M-x zapiska-study-mode   ; Open study window
;;   M-x hydra-zapiska/body   ; Main menu
;;
;; Quick start:
;;   1. Add package to load-path: (add-to-list 'load-path "~/Projects/zapiska")
;;   2. Load package: (require 'zapiska)
;;   3. Bind keys: See PLAN.org for keybinding examples
;;
;;; Code:

;; ======== DEPENDENCIES ========

(require 'cl-lib)
(require 'hydra)
(require 'evil)

;; ======== CUSTOMIZATION GROUP ========

(defgroup zapiska nil
  "Russian vocabulary learning system."
  :group 'applications
  :prefix "zapiska-")

(defcustom zapiska-data-directory
  (expand-file-name "zapiska-data" (file-name-directory (or load-file-name buffer-file-name)))
  "Directory where vocabulary data is stored."
  :type 'directory
  :group 'zapiska)

(defcustom zapiska-show-keyboard-in-quiz t
  "Whether to automatically show keyboard reference during quizzes."
  :type 'boolean
  :group 'zapiska)

(defcustom zapiska-study-word-count 7
  "Number of words to display in study mode."
  :type 'integer
  :group 'zapiska)

(defcustom zapiska-study-auto-refresh-interval nil
  "Seconds between auto-refresh in study mode. Nil to disable auto-refresh."
  :type '(choice (const :tag "Disabled" nil)
                 (integer :tag "Seconds"))
  :group 'zapiska)

;; ======== DATA STRUCTURES ========

(defvar zapiska-db (make-hash-table :test 'equal)
  "Hash table mapping word IDs (UUIDs) to vocabulary entry plists.

Each entry has the structure:
  (:id \"uuid-string\"
   :russian \"A;>2>\"
   :english \"word\"
   :mastery-level 0        ; 0-5 scale
   :easiness 2.5           ; SM-2 factor (1.3-2.5)
   :repetitions 0          ; Successful recall count
   :interval 0             ; Days until next review
   :next-review nil        ; Timestamp or nil
   :last-reviewed nil      ; Last review timestamp
   :times-seen 0           ; Total quiz appearances
   :times-correct 0        ; Total correct answers
   :created (current-time)
   :notes \"\")")

(defvar zapiska-stats
  '(:total-sessions 0
    :total-reviews 0
    :total-correct 0
    :current-streak 0
    :longest-streak 0
    :last-session nil)
  "Global statistics plist tracking learning progress.")

(defvar zapiska-quiz-session nil
  "Current quiz session state plist.

Structure:
  (:active t
   :queue (list of word IDs)
   :incorrect-queue nil
   :current-word-id nil
   :direction 'russian-to-english  ; or 'english-to-russian
   :start-time (current-time)
   :words-reviewed 0
   :correct-count 0)")

(defvar zapiska-study-timer nil
  "Timer for auto-refresh in study mode.")

;; ======== CUSTOM FACES ========

(defface zapiska-quiz-word
  '((t :height 2.0 :weight bold :foreground "#cbe3e7"))
  "Face for quiz word display."
  :group 'zapiska)

(defface zapiska-correct
  '((t :foreground "#62d196" :weight bold))
  "Face for correct answers."
  :group 'zapiska)

(defface zapiska-incorrect
  '((t :foreground "#ff8080" :weight bold))
  "Face for incorrect answers."
  :group 'zapiska)

(defface zapiska-mastery-high
  '((t :foreground "#62d196"))
  "Face for words with mastery level 4-5."
  :group 'zapiska)

(defface zapiska-mastery-medium
  '((t :foreground "#ffd580"))
  "Face for words with mastery level 2-3."
  :group 'zapiska)

(defface zapiska-mastery-low
  '((t :foreground "#ff8080"))
  "Face for words with mastery level 0-1."
  :group 'zapiska)

;; ======== UTILITY FUNCTIONS ========

(defun zapiska--generate-uuid ()
  "Generate a simple UUID for word entries."
  (format "%08x-%08x-%08x"
            (random (expt 2 32))  ; Random 32-bit number (8 hex digits)
            (random (expt 2 32))
            (time-convert (current-time) 'integer)))

(defun zapiska--hash-table-to-alist (hash-table)
  "Convert HASH-TABLE to an association list.

This is used for serializing the database to disk."
  (let (alist)
    (maphash (lambda (key value)
               (push (cons key value) alist))
             hash-table)
    alist))

(defun zapiska--alist-to-hash-table (alist)
  "Convert ALIST to a hash table.

This is used for loading the database from disk."
  (let ((zapiska-data (make-hash-table)))
    (dolist (row alist)
      (puthash (car row) (cdr row) zapiska-data))
    zapiska-data))

(defun zapiska--get-data-file ()
  "Return the full path to the vocabulary data file."
  (expand-file-name "vocab-data.el" zapiska-data-directory))


(defun zapiska-add-test-data ()
  "Add some test vocabulary words for development.
Only for development use."
  (interactive)
  (let ((test-words
         '(("слово" "word" "Common noun")
           ("привет" "hello" "Greeting")
           ("спасибо" "thank you" "Polite expression")
           ("пожалуйста" "please/you're welcome" "Polite expression")
           ("хорошо" "good/well" "Adjective/adverb")
           ("день" "day" "Noun")
           ("ночь" "night" "Noun"))))
    (dolist (word test-words)
      (let* ((russian (nth 0 word))
             (english (nth 1 word))
             (notes (nth 2 word))
             (id (zapiska--generate-uuid))
             (entry (list :id id
                          :russian russian
                          :english english
                          :mastery-level 0
                          :easiness 2.5
                          :repetitions 0
                          :interval 0
                          :next-review nil
                          :last-reviewed nil
                          :times-seen 0
                          :times-correct 0
                          :created (current-time)
                          :notes notes)))
        (puthash id entry zapiska-db)))
    (message "Added %d test words" (length test-words))))

(defun zapiska-clear-all-data ()
  "Clear all vocabulary data. USE WITH CAUTION!"
  (interactive)
  (when (yes-or-no-p "Really clear ALL vocabulary data? ")
    (setq zapiska-db (make-hash-table :test 'equal))
    (message "All data cleared")))

;; ======== DATA PERSISTENCE ========

(defun zapiska-save-data ()
  "Save vocabulary database and statistics to disk.

Creates a backup before saving if file already exists."
  (interactive)
  ;; TODO: Implement data saving
  ;; Steps:
  ;; 1. Ensure zapiska-data-directory exists (use make-directory with parents arg)
  ;; 2. Convert zapiska-db to alist
  ;; 3. Use with-temp-file to write to data file
  ;; 4. Use prin1 to serialize the data structures
  ;; 5. Add backup logic (optional for Phase 1)
  (message "Saving vocabulary data..."))

(defun zapiska-load-data ()
  "Load vocabulary database and statistics from disk.

Creates empty database if file doesn't exist."
  (interactive)
  ;; TODO: Implement data loading
  ;; Steps:
  ;; 1. Check if data file exists
  ;; 2. If exists, use load to read the file
  ;; 3. Convert loaded alist back to hash table
  ;; 4. If doesn't exist, initialize empty structures
  (message "Loading vocabulary data..."))

;; ======== VOCABULARY LIST MODE ========

(defvar zapiska-list-mode-map
  (let ((map (make-sparse-keymap)))
    ;; TODO: Define keybindings using evil-define-key
    ;; 'normal state bindings: a, d, e, RET, r, q, i, x, X
    map)
  "Keymap for `zapiska-list-mode'.")

(define-derived-mode zapiska-list-mode tabulated-list-mode "Zapiska-List"
  "Major mode for managing Russian vocabulary list.

\\{zapiska-list-mode-map}"
  ;; TODO: Set up tabulated-list-mode
  ;; Steps:
  ;; 1. Define tabulated-list-format (columns: Russian, English, Mastery, Next Review, Seen, Accuracy)
  ;; 2. Set tabulated-list-entries to a function that generates entries from zapiska-db
  ;; 3. Call tabulated-list-init-header
  ;; 4. Set up evil keybindings
  )

(defun zapiska-open-list ()
  "Open the vocabulary list buffer."
  (interactive)
  ;; TODO: Implement list opening
  ;; Steps:
  ;; 1. Switch to or create buffer named "*Zapiska List*"
  ;; 2. Enable zapiska-list-mode
  ;; 3. Call tabulated-list-print to populate
  )

(defun zapiska-add-word ()
  "Add a new word to the vocabulary database.

Prompts for Russian word, English translation, and optional notes."
  (interactive)
  ;; TODO: Implement word addition
  ;; Steps:
  ;; 1. Prompt for russian word (read-string)
  ;; 2. Prompt for english translation
  ;; 3. Prompt for optional notes
  ;; 4. Generate UUID
  ;; 5. Create entry plist with default values
  ;; 6. Add to zapiska-db
  ;; 7. Save data
  ;; 8. Refresh list if in list mode
  (message "Adding word..."))

(defun zapiska-delete-word ()
  "Delete the word at point in the vocabulary list."
  (interactive)
  ;; TODO: Implement word deletion
  ;; Steps:
  ;; 1. Get word ID at current point (from tabulated-list-get-id)
  ;; 2. Confirm with user (yes-or-no-p)
  ;; 3. Remove from zapiska-db (remhash)
  ;; 4. Save data
  ;; 5. Refresh list
  (message "Deleting word..."))

(defun zapiska-edit-word ()
  "Edit the word at point in the vocabulary list."
  (interactive)
  ;; TODO: Implement word editing
  ;; Steps:
  ;; 1. Get word entry at point
  ;; 2. Prompt for new values (with current values as defaults)
  ;; 3. Update entry in zapiska-db
  ;; 4. Save data
  ;; 5. Refresh list
  (message "Editing word..."))

(defun zapiska-list-refresh ()
  "Refresh the vocabulary list display."
  (interactive)
  ;; TODO: Reload and redisplay the list
  ;; Hint: Call tabulated-list-print with revert arg
  (message "Refreshing list..."))

;; ======== QUIZ MODE ========

(defvar zapiska-quiz-mode-map
  (let ((map (make-sparse-keymap)))
    ;; TODO: Define keybindings
    ;; Consider: n (next/skip), k (keyboard), s (stats), q (quit)
    map)
  "Keymap for `zapiska-quiz-mode'.")

(define-derived-mode zapiska-quiz-mode special-mode "Zapiska-Quiz"
  "Major mode for vocabulary quiz sessions.

\\{zapiska-quiz-mode-map}"
  ;; TODO: Set up quiz mode
  ;; This mode displays the current word and handles quiz flow
  (setq buffer-read-only t))

(defun zapiska-start-quiz (filter)
  "Start a quiz session with words matching FILTER.

FILTER can be:
  'all          - All words
  'due          - Words due for review
  'new          - Words never reviewed
  'low-mastery  - Words with mastery level < 3"
  (interactive)
  ;; TODO: Implement quiz start
  ;; Steps:
  ;; 1. Filter words based on FILTER argument
  ;; 2. Shuffle filtered words to create queue
  ;; 3. Initialize zapiska-quiz-session plist
  ;; 4. Switch to quiz buffer "*Zapiska Quiz*"
  ;; 5. Enable zapiska-quiz-mode
  ;; 6. Display first word
  (message "Starting quiz with filter: %s" filter))

(defun zapiska-quiz-next-word ()
  "Display the next word in the quiz session."
  ;; TODO: Implement next word logic
  ;; Steps:
  ;; 1. Check if queue is empty (if so, show completion message)
  ;; 2. Pop next word ID from queue
  ;; 3. Randomly choose direction (russian-to-english or english-to-russian)
  ;; 4. Update quiz session state
  ;; 5. Display word in buffer with large font
  ;; 6. Prompt for answer in minibuffer
  )

(defun zapiska-quiz-check-answer (word-entry user-answer)
  "Check USER-ANSWER against WORD-ENTRY and provide feedback.

Returns t if correct, nil if incorrect."
  ;; TODO: Implement answer checking
  ;; Steps:
  ;; 1. Get correct answer based on quiz direction
  ;; 2. Normalize both answers (downcase, trim whitespace)
  ;; 3. Compare for exact match
  ;; 4. Update word statistics (times-seen, times-correct)
  ;; 5. Call SM-2 update function
  ;; 6. Display feedback (correct/incorrect with correct answer)
  ;; 7. If incorrect, add back to queue
  ;; 8. After brief pause, call zapiska-quiz-next-word
  )

(defun zapiska-quit-quiz ()
  "End the current quiz session and show summary."
  (interactive)
  ;; TODO: Implement quiz exit
  ;; Steps:
  ;; 1. Calculate session statistics
  ;; 2. Update zapiska-stats
  ;; 3. Display summary message
  ;; 4. Clear zapiska-quiz-session
  ;; 5. Save data
  ;; 6. Kill quiz buffer
  (message "Quitting quiz..."))

;; ======== STUDY MODE ========

(defvar zapiska-study-mode-map
  (let ((map (make-sparse-keymap)))
    ;; TODO: Define keybindings
    ;; r - refresh/next batch
    ;; q - quit
    map)
  "Keymap for `zapiska-study-mode'.")

(define-derived-mode zapiska-study-mode special-mode "Zapiska-Study"
  "Major mode for passive vocabulary study.

Displays a selection of words for passive review while working.

\\{zapiska-study-mode-map}"
  ;; TODO: Set up study mode
  (setq buffer-read-only t)
  ;; Set up auto-refresh timer if configured
  (when zapiska-study-auto-refresh-interval
    (setq zapiska-study-timer
          (run-at-time zapiska-study-auto-refresh-interval
                       zapiska-study-auto-refresh-interval
                       #'zapiska-study-refresh))))

(defun zapiska-study-mode-open ()
  "Open study mode in a side window.

Displays a random selection of words for passive learning."
  (interactive)
  ;; TODO: Implement study mode opening
  ;; Steps:
  ;; 1. Get or create buffer "*Zapiska Study*"
  ;; 2. Enable zapiska-study-mode
  ;; 3. Display buffer in side window (use display-buffer-in-side-window)
  ;; 4. Call zapiska-study-refresh to populate
  (message "Opening study mode..."))

(defun zapiska-study-refresh ()
  "Refresh study mode with a new batch of words."
  (interactive)
  ;; TODO: Implement study display
  ;; Steps:
  ;; 1. Get random selection of words (or due words)
  ;; 2. Clear buffer (let inhibit-read-only to t)
  ;; 3. Insert header
  ;; 4. Insert each word with Russian -> English format
  ;; 5. Add footer with count and keybinding hints
  (message "Refreshing study words..."))

(defun zapiska-study-quit ()
  "Quit study mode and kill the study buffer."
  (interactive)
  ;; TODO: Implement study quit
  ;; 1. Cancel auto-refresh timer if active
  ;; 2. Kill buffer
  (when zapiska-study-timer
    (cancel-timer zapiska-study-timer)
    (setq zapiska-study-timer nil))
  (message "Closing study mode..."))

;; ======== SPACED REPETITION (SM-2 ALGORITHM) ========

(defun zapiska-update-word-schedule (word-entry quality)
  "Update WORD-ENTRY's schedule using SM-2 algorithm based on QUALITY.

QUALITY is 5 for correct answer, 2 for incorrect answer.

Updates the following fields in word-entry:
  - :easiness
  - :repetitions
  - :interval
  - :next-review
  - :mastery-level
  - :last-reviewed"
  ;; TODO: Implement SM-2 algorithm
  ;; See PLAN.org for the formula
  ;; Steps:
  ;; 1. Calculate new easiness factor
  ;; 2. Update repetitions (increment or reset to 0)
  ;; 3. Calculate new interval based on repetitions
  ;; 4. Set next-review timestamp (current-time + interval days)
  ;; 5. Update mastery-level based on repetitions and easiness
  ;; 6. Set last-reviewed to current-time
  ;; 7. Return modified word-entry
  )

(defun zapiska-calculate-mastery-level (repetitions easiness)
  "Calculate mastery level (0-5) based on REPETITIONS and EASINESS.

Level 0: Never reviewed
Level 1: 1-2 repetitions
Level 2: 3-5 repetitions
Level 3: 6-10 repetitions, E > 2.0
Level 4: 11-20 repetitions, E > 2.2
Level 5: 21+ repetitions, E > 2.4"
  ;; TODO: Implement mastery level calculation
  ;; Use cond to check repetitions and easiness ranges
  0)

;; ======== STATISTICS ========

(defun zapiska-show-statistics ()
  "Display statistics dashboard in a new buffer."
  (interactive)
  ;; TODO: Implement statistics display
  ;; Steps:
  ;; 1. Create/switch to "*Zapiska Statistics*" buffer
  ;; 2. Calculate all statistics from zapiska-db and zapiska-stats
  ;; 3. Insert formatted statistics (see PLAN.org for layout)
  ;; 4. Make buffer read-only
  (message "Showing statistics..."))

;; ======== CYRILLIC KEYBOARD REFERENCE ========

(defvar zapiska-keyboard-buffer-name "*Zapiska Keyboard*"
  "Name of the Cyrillic keyboard reference buffer.")

(defun zapiska-toggle-keyboard-reference ()
  "Toggle the Cyrillic keyboard reference window."
  (interactive)
  ;; TODO: Implement keyboard toggle
  ;; Steps:
  ;; 1. Check if keyboard buffer exists and is visible
  ;; 2. If visible, hide it (delete-window)
  ;; 3. If not visible, show it (zapiska-show-keyboard-reference)
  (message "Toggling keyboard reference..."))

(defun zapiska-show-keyboard-reference ()
  "Display the Cyrillic keyboard reference in a side window."
  ;; TODO: Implement keyboard display
  ;; Steps:
  ;; 1. Get or create keyboard buffer
  ;; 2. Insert keyboard layout (see PLAN.org for ASCII layout)
  ;; 3. Make buffer read-only
  ;; 4. Display in side window (bottom, 30% height)
  )

;; ======== IMPORT/EXPORT ========

(defun zapiska-import-csv (file)
  "Import vocabulary from CSV FILE.

Expected format: russian,english,notes"
  (interactive "fImport CSV file: ")
  ;; TODO: Implement CSV import
  ;; Steps:
  ;; 1. Read file contents
  ;; 2. Parse CSV (can use simple split on newlines and commas)
  ;; 3. For each row, create word entry with default values
  ;; 4. Add to zapiska-db
  ;; 5. Save data
  ;; 6. Show success message with count
  (message "Importing from %s..." file))

(defun zapiska-export-csv (file)
  "Export vocabulary to CSV FILE.

Format: russian,english,notes"
  (interactive "FExport to CSV file: ")
  ;; TODO: Implement CSV export
  ;; Steps:
  ;; 1. Open file for writing
  ;; 2. Write header row
  ;; 3. Iterate over zapiska-db, write each word as CSV row
  ;; 4. Close file
  ;; 5. Show success message
  (message "Exporting to %s..." file))

(defun zapiska-export-anki (file)
  "Export vocabulary to Anki TSV FILE.

Format: russian<TAB>english<TAB>notes"
  (interactive "FExport to Anki TSV file: ")
  ;; TODO: Implement Anki export (similar to CSV but tab-separated)
  (message "Exporting to Anki format at %s..." file))

;; ======== HYDRA MENUS ========

(defhydra hydra-zapiska (:color blue :hint nil)
  "
^Quiz^              ^Manage^           ^Stats^             ^Import/Export^    ^Study^
                                                                                          
_q_: Quiz all       _l_: Vocab list    _s_: Statistics     _i_: Import CSV    _S_: Study mode
_d_: Quiz due       _a_: Add word      _h_: Session hist   _x_: Export CSV
_n_: Quiz new       _k_: Keyboard                          _X_: Export Anki
_m_: Low mastery
"
  ("q" (zapiska-start-quiz 'all) "quiz all")
  ("d" (zapiska-start-quiz 'due) "quiz due")
  ("n" (zapiska-start-quiz 'new) "quiz new")
  ("m" (zapiska-start-quiz 'low-mastery) "low mastery")
  ("l" zapiska-open-list "vocab list")
  ("a" zapiska-add-word "add word")
  ("k" zapiska-toggle-keyboard-reference "keyboard")
  ("s" zapiska-show-statistics "statistics")
  ("h" (message "Session history not yet implemented") "session hist")
  ("i" zapiska-import-csv "import csv")
  ("x" zapiska-export-csv "export csv")
  ("X" zapiska-export-anki "export anki")
  ("S" zapiska-study-mode-open "study mode"))

(defhydra hydra-zapiska-quiz (:color pink :hint nil)
  "
^Control^          ^View^
                                
_n_: Next (skip)   _k_: Keyboard
_q_: Quit quiz     _s_: Stats
"
  ("n" (message "Skip not yet implemented") "skip")
  ("k" zapiska-toggle-keyboard-reference "keyboard")
  ("s" zapiska-show-statistics "stats")
  ("q" zapiska-quit-quiz "quit" :exit t))

;; ======== DISPLAY BUFFER CONFIGURATION ========

;; Configure side windows for study mode and keyboard reference
(add-to-list 'display-buffer-alist
             '("^\\*Zapiska Study\\*"
               (display-buffer-in-side-window)
               (side . right)
               (window-width . 0.28)))

(add-to-list 'display-buffer-alist
             '("^\\*Zapiska Keyboard\\*"
               (display-buffer-in-side-window)
               (side . bottom)
               (window-height . 0.3)))

;; ======== INITIALIZATION ========

;; Load data when package is loaded
(zapiska-load-data)

(provide 'zapiska)
;;; zapiska.el ends here
