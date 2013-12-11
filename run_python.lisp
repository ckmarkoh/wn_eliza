(require "asdf")

(defun runpy_interface(word)
	"Lisp-Python interface"
	(string_to_list
	  (remove_str_char 	
			(	runpy_wordnet (remove_str_char (list_to_string word ) '(#\( #\))))
		'(#\linefeed #\return #\$ #\;)
		)
	)
)

(defun runpy_wordnet(str_word)
	"Lisp-Python interface for wordnet"
		(with-output-to-string (asdf::*verbose-out*) 
			(asdf:run-shell-command 
				(concatenate 'string "python wordnetpy.py " str_word
				)
			)
		)
)


(defun remove_str_char(str_to_rm str_a)
		(dolist (char_a str_a)
			(setf str_to_rm (remove char_a str_to_rm))
		)
		str_to_rm
)


(defun list_to_string (lst)
	(string-downcase (princ-to-string lst) )
)

(defun string_to_list (str)
	(if (not (streamp str))
		(string_to_list (make-string-input-stream str))
		(if (listen str)
			(cons (read str) (string_to_list str)) 
			nil
		)
	)
)

