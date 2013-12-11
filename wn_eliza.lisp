(load "run_python")
(load "all_rules")

(defconstant fail nil 
	"Indicates pat_match failure"
)

(defconstant no_bindings '((t . t )) 
	"Indicates pat_match success, with no variables."
)

(defconstant pydelimiter '(?-?)
	"Delimiter to catch python program output"
)

(defun starts-with (list x)
	"Is x a list whose first element is x?"
	(and (consp list) (eql (first list) x))
)

(defun variable_p (x)
	"Is x a variable? (a symbol beginning with '?')?"
	 (and (symbolp x) (equal (char (symbol-name x ) 0 ) #\?))
)

(defun get_binding (var bindings)
	"Find a (variable. value) pair in a binding list."
	(assoc var bindings)
)

(defun binding_val (bindings)
	"Get the value part of a single binding."
	(cdr bindings)
)

(defun lookup (var bindings)
	"Get the value part (for var) from a binding list."
	(binding_val (get_binding var bindings))
)

(defun extend_bindings(var val bindings)
	"Add a (var . value) pair to a binding list."
	(cons (cons var val) 
		(if (eq bindings no_bindings)
			nil
			bindings)
	)
)

(defun pat_match (pattern input &optional (bindings no_bindings))
	"Match pattern against input in the context of the bindings."
	(cond ((eq bindings fail) fail)
		((variable_p pattern) (match_variable pattern input bindings))
		((eql pattern input) bindings)
		((segment_pattern_p pattern) (segment_match pattern input bindings))
		((and (consp pattern) (consp input)) 
			(pat_match (rest pattern) (rest input)
						(pat_match (first pattern) (first input) bindings)))
		(t fail)
	)	
)

(defun match_variable (var input bindings)
	"Does VAR match input? Uses (or updates) and returns bindings."
	(let ((binding (get_binding var bindings)))
		(cond ((not binding) (extend_bindings var input bindings))
			((equal input (binding_val binding)) bindings)
			(t fail)
		)
	)
)
		

(defun segment_pattern_p (pattern)
	"Is this a segment matching pattern: ((?* var) .pat)"
	(and (consp pattern)
		(starts-with (first pattern) '?*)
	)
)

(defun segment_match (pattern input bindings &optional (start 0))
	"Match the segment pattern ((?* var) .pat) against input."
	(let ((var (second (first pattern))) (pat (rest pattern)))
		(if (null pat)
			(match_variable var input bindings)
			(let ((pos (position (first pat) input :start start :test #'equal)))
				(if (null pos)
					fail
					(let ((b2 (pat_match pat (subseq input pos)
							(match_variable var (subseq input 0 pos) bindings))))
						(if (eq b2 fail)
							(segment_match pattern input bindings (+ pos 1))
							b2
						)
					)
				)
			)
		)
	)
)

(defun eliza()
	"Respond to user input using pattern matching rules."
	(loop 
		(print 'eliza>)
		(write  (flatten (use_rules (read)))  )
	)
)

(defun use_rules(input)
	(let ((wn_result (use_wn_rules input))) 
		(if (null wn_result)
			(use_eliza_rules input) 
			wn_result
		)
	)
)

(defun use_wn_rules(input) ;TODO response to wordnet rules.
	"Find some rule with which to transform the input."
	(some #'(lambda (rule)
			(let* (  (wn_pattern  (rule_pattern rule))
					(wn_response  (rule_responses rule))
					(result (pat_match wn_pattern input))
				)
				(if (not (eq result fail))
				 	(let ((py_result (pat_match `((?* ?x) ,@pydelimiter (?* ?a) ,@pydelimiter (?* ?y))
												(runpy_interface 
														 (binding_val (get_binding '?q result))
												) 
									) 
							))
						;(print py_result)
						;(print result)
						(if (not (eq py_result fail))
							(sublis (append py_result result) (random_elt wn_response ))
						)
					)
				)
		)
	) *wn_rules*)
)

(defun use_eliza_rules(input)
	"Find some rule with which to transform the input."
	(some #'(lambda (rule)
				(let ((result (pat_match(rule_pattern rule) input)))
					(if (not (eq result fail))
						(sublis (switch_viewpoint result)
								(random_elt (rule_responses rule))
						)
					)
				)
	) *eliza_rules*)
)
	
(defun switch_viewpoint (words)
	"Change I to you and vice versa, and so on."
	(SUBLIS '((I . you) (you . I) (me .you) (am .are)) words )
)

(defun flatten(lst)
	"Append together elements (or lists) in the list."
	(if (null lst)
		'(Sorry... I don't understand what you are saying. ) ;TODO response to nil result. 
		(mappend #'mklist lst)
	)
)

(defun mklist(x)
	"Return x if it is a list, otherwise (x)."
	(if (listp x)
		x
		(list x)
	)
)
(defun mappend (fn lst)
	"Apply fn to each element of list and append the results."
	(apply #'append (mapcar fn lst)	)
)
	
(defun random_elt (choices)
  "Choose an element from a list at random."
  (elt choices (random (length choices)))
)

(defun rule_pattern (rule) (first rule))
(defun rule_responses (rule) (rest rule))

;;; ==============================
;	? > (eliza)
;	ELIZA> (what is pig ?)
;	(PIG IS DOMESTIC SWINE)
;	ELIZA> (what is doga ?)
;	(THE MEANING OF DOGA IS NOT IN WORDNET)
;	ELIZA> (what is dog ?)
;	(DOG IS A MEMBER OF THE GENUS CANIS (PROBABLY DESCENDED FROM THE COMMON WOLF) THAT HAS BEEN DOMESTICATED BY MAN SINCE PREHISTORIC TIMES OCCURS IN MANY BREEDS)
;;; ==============================
