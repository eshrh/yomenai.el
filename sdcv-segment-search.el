(require 'sdcv)
(require 'mecab)
(require 'subr-x)

(defun select-word (idx len)
  (interactive)
  (beginning-of-line)
  (set-mark (+ idx (point)))
  (goto-char (+ idx (- len 1) (point))))

(defun getindex ()
  (save-excursion
    (setq curpoint (point))
    (move-beginning-of-line 'nil)
    (setq startpoint (point)))
  (- curpoint startpoint))

(defun search (i idx cur)
  (let ((item (car idx)))
    (if (<= item i) (- cur 1)
      (search i (cdr idx) (- cur 1)))))

(defun find-starts (words line start idcs)
  (interactive)
  (if (null words)
    idcs
    (find-starts (cdr words)
                 line
                 (if (null idcs) 0 (+ 1 (car idcs)))
                 (cons (string-match (car (car words)) line start) idcs))))

(defun mecab-get-word ()
  (interactive)
  (let* ((mecab (mecab-new "/usr/lib/mecab/dic/ipadic"))
         (line (string-trim (thing-at-point 'line t)))
         (words (mapcar (lambda (item)
                          (split-string item "\t"))
                        (butlast
                         (split-string
                          (mecab-sparse-to-string mecab line) "\n" t))))
         (indices (find-starts words line 0 'nil))
         (wordidx (search
                   (getindex)
                   indices
                   (length indices)))
         (token (nth wordidx words)))
    (select-word (nth wordidx (reverse indices))
                 (length (car token)))
    (cons (car token) (split-string (cadr token) ","))))


(defun sdcv-segment-search ()
  "Segment sentence with mecab and search for word at point"
  (interactive)
  (let ((word (mecab-get-word)))
    (sdcv-search-input (nth 7 word))))