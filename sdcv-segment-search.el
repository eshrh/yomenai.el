;;; sdcv-segment-search.el --- search for words using sdcv words
;;; by segmenting them with mecab
;;; Commentary:
;;; author: github.com/eshrh

(require 'sdcv)
(require 'mecab)
(require 'subr-x)

;;; Code:
(defun select-word (idx len)
  "Highlight a word beginning at IDX and having length LEN."
  (beginning-of-line)
  (set-mark (+ idx (point)))
  (goto-char (+ idx (- len 1)
                (point))))

(defun getindex ()
  "Get the character position of the cursor."
  (save-excursion
    (setq curpoint (point))
    (move-beginning-of-line 'nil)
    (setq startpoint (point)))
  (- curpoint startpoint))

(defun search (i idx cur)
  "Find the index of the current word in the list of segments.
Given a list of indices IDX which are the starting positions of words
and index I which is where the word starts, return the largest index of the item
in IDX lesser than or equal to I."
  (let ((item (car idx)))
    (if (<= item i)
        (- cur 1)
      (search i (cdr idx)
              (- cur 1)))))

(defun find-starts (words line start idcs)
  "Find the index each segmented word begins on.
WORDS is the list of segments, LINE is the raw text, START is an accumulator
variable beginning at 0 for keeping track of `string-match` start points, IDCS
is the result array to be returned and should start at nil."
  (if (null words) idcs (find-starts (cdr words)
                                     line
                                     (if (null idcs) 0 (+ 1 (car idcs)))
                                     (cons (string-match (car (car words))
                                                         line start) idcs))))

(defcustom mecab-dictionary-path "/usr/lib/mecab/dic/ipadic"
  "Path to mecab dictionary."
  :type 'string
  :require 'mecab)

(defun mecab-get-word ()
  "Return the mecab output for the word the cursor is currently on."
  (let* ((mecab (mecab-new mecab-dictionary-path))
         (line (string-trim (thing-at-point 'line t)))
         (words (mapcar (lambda (item)
                          (split-string item "\t"))
                        (butlast (split-string
                                  (mecab-sparse-to-string mecab line) "\n" t))))
         (indices (find-starts words line 0 'nil))
         (wordidx (search (getindex) indices (length indices)))
         (token (nth wordidx words)))
    (select-word (nth wordidx (reverse indices))
                 (length (car token)))
    (cons (car token)
          (split-string (cadr token) ","))))

(defun shr-render-sdcv ()
  "Use shr to render any html or div tags in an sdcv output buffer."
  (interactive)
  (with-current-buffer "*SDCV"
    (let* ((buf
            (buffer-substring-no-properties
             (point-min)
             (point-max)))
           (divmatch (string-match "<div" buf))
           (htmlmatch (string-match "<html" buf))
           (inhibit-read-only 1))
      (cond (htmlmatch (shr-render-region
                        htmlmatch (+ 8 (string-match "/html>" buf)))
                       (shr-render-sdcv))
            (divmatch (shr-render-region
                       divmatch (+ 6 (string-match "/div>" buf)))
                      (shr-render-sdcv))))))

(defun sdcv-segment-search ()
  "Segment sentence with mecab and search for word at point."
  (interactive)
  (let ((word (mecab-get-word)))
    (sdcv-search-input (nth 7 word))))

(add-hook 'sdcv-mode-reinit-hook #'deactivate-mark)
(add-hook 'sdcv-mode-reinit-hook (lambda ()
                                   (goto-char (point-min))))
(add-hook 'sdcv-mode-reinit-hook #'shr-render-sdcv)

(provide 'sdcv-segment-search)
;;; sdcv-segment-search.el ends here
