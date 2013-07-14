
;;;  This is the top level of the lisp to postscript routines.


;; ps is the top level entry.  The argument is the file name.

(defun ps (infile outfile)
    ;; Initialize global structures
    (setf main-program nil)
    (setf error-count 0)
    (setf lexical-vars nil)
    (setf next-frame 1)
    (setf current-frame '**main**)
    (setf current-fn '**main**)
    (setf frame-table (make-hash-table))
    (setf to-compile nil)
    (setf to-init nil)
    
    ;; These hash tables contain the user environment
    
    (setf env-list (list (make-hash-table)))
    (setf main-table (car env-list))
    (setf current-table main-table)
    (setf current-env (list main-table))

    (setf ps-file-header (list "%!"))
    (setf ps-file-extension ".ps")

    ;; Now, interpret the contents of the file.  
    (interpret-file infile)

    ;; All function and macro definitions have been sucked in - now compile
    ;; everything down.
    
    ;; Compile the main program as a giant progn.

    (setf fn-code (list '(null)))
    (setf init-code (list '(null)))

    (setf main-env
	  (setf current-env (reverse env-list)))

    (setf code-stream (list '(null)))

    (ps-compile-novalue (cons 'progn (reverse main-program)))

    (setf main-code code-stream)

    (while (or to-compile to-init)
      (setf code-stream fn-code)
      (while to-compile
	 (compile-function (pop to-compile)))
      (setf fn-code code-stream)

      (setf code-stream init-code)
      (while to-init
	 (compile-init-var (pop to-init)))
      (setf init-code code-stream))

      (if (= error-count 0)
	  (progn
	    (flow-analysis) 
	    (setf code-stream (list '(null)))
	    (maphash #'init-non-recursives frame-table)
	    (setf dict-code code-stream)
	    
	    (with-open-file
	     (ps-output outfile
			:direction :output)
	     (ps-write-code))
	    outfile)
	nil) )

(defvar preview-postscript-executable "c:/Downloads-Updates/drawing/ghostscript/gsview-4.6/bin/gsview32.exe")

(defun ps-and-show (infile outfile)
  (let* ( (result (ps infile outfile)) )
    (if result
	(progn
	  (run-program preview-postscript-executable :arguments (list "-e" result) :wait nil)
	  result) ) ) )

(defun init-non-recursives (x v)
  (if (and (att frame-table x 'non-recursive) (not (eq x '**main**))) 
      (emit `(static-alloc ,x))))

;;  Here, a file is read in.  Use the lisp reader, read form by form, and
;;  hand each form to ps-interpret-form.

(defun interpret-file (fname &key filter)
  (let (ps-form)
    (with-open-file
	(ps-input fname :direction :input)
	(while (not (eql (setf ps-form (read ps-input nil '***PSEOF***))
			 '***PSEOF***))
	  (if (or (not filter) (funcall filter ps-form))
	       (ps-interpret-form ps-form))))))

(defun filter-out-eps-load-library (fm)
  (if (listp fm)
      (let ( (sym (car fm)) )
	(case sym
	  ((load library eps) nil)
	  (otherwise t) ) )
    t) )

;;  Decide what to do with a random form at the top level.  There are a
;;  few special cases, otherwise just tack things onto the end of the
;;  main program.

(defun ps-interpret-form (fm)
    (when (consp fm)
	  (let ((fn (car fm)))
	       (cond ((eq fn 'load)
		      (ps-load (cadr fm)))  ;  recursively load another file
		                            ;  Note full file name needed here
		     ((eq fn 'load-filtered)
		      (ps-load (cadr fm) :filter (eval (caddr fm))) )  ;  
		     ((eq fn 'defun)
		      (def-ps-fun (cdr fm))) ; save all defuns
		     ((eq fn 'defmacro)
		      (def-ps-mac (cdr fm))) ; and macros
                     ((eq fn 'defconstant)
		      (def-ps-constant (cdr fm))) ; and constants
		     ((eq fn 'defvar)
		      (def-ps-var (cdr fm))) ; and defvars
		     ((eq fn 'eval)
		      (eval (cadr fm))) ; this is the escape to lisp
		                        ; Call the lisp evaluator.
		     ((eq fn 'eps)
		      (format t "Evaluating eps form with args ~A" (cdr fm))
		      (apply #'set-eps-header (cdr fm)))
		     ((eq fn 'quote)) ; do nothing
		     ((eq fn 'progn)
		      (dolist (progn-form (cdr fm))
			(ps-interpret-form progn-form) ) )
		     ((ps-macro fn)     ; Allow top level macros
		      (ps-interpret-form (ps-macro-expand fm)))
		     ((eq fn 'library)
		      (ps-load-library (cadr fm)))
		     ((eq current-table main-table)
		      (attach-to-main fm))
		     (t (ps-error "Library file must only have definitions"
				  fm))))))

(defun make-font-header (header fonts)
  (if fonts
      (let ( (font-header header) )
	(dolist (font fonts)
	  (setq font-header (concatenate 'string font-header " " font)) )
	font-header)
    nil) )

;; EPS header and bounding box
(defun set-eps-header (x1 y1 x2 y2 &rest document-fonts)
  (setf ps-file-extension ".eps")
  (setf ps-file-header 
	(list
	 "%!PS-Adobe-3.0 EPSF-3.0"
	 (format nil "%%BoundingBox: ~D ~D ~D ~D" x1 y1 x2 y2)
	 (make-font-header "%%DocumentFonts:" document-fonts)
	 (make-font-header "%%DocumentNeededFonts:" document-fonts)
	 "%%EndComments"
	 ) ) )

;; This is where non special top level forms go

(defun attach-to-main (fm)
   (push fm main-program))

;; This is a (very) crude error handler.  You just get a message and (probably)
;; enough surrounding context to help locate the problem.

(defun ps-error (&rest messages)
  (let ((*print-level* 3)
	(*print-length* 5))
   (format t "Error in postscript translation~%")
   (for (:in x messages)
        (:do (format t "~A~%" x)))
   (incf error-count)))

;; Here is where defun's in the postscript world go.  They just get saved
;; in ps-funs (where the will be compiled later) and enough information
;; to allow proper compilation of calls to the function is saved in
;; the user-ps-funs hash table.

(defun def-ps-fun (fm)
    (let ((fname (car fm))   ; name of the function
	  (args (cadr fm))   ; argument list
	  (body (cddr fm))   ; number of values returned (an integer) / body
          (nres 0))          ; number of results
       (if (numberp (car body))
	   (setf nres (car body) body (cdr body))
	   (cond ((eq *assume-0-res* 'warn)
		  (format t "Assuming ~A returns no result~%" fname))
	         ((not *assume-0-res*)
	 (ps-error "Postscript functions must declare values returned" fm))))
       (new-name fname)
       (define-name current-table fname 'user-function)
       (put-att current-table fname 'args args)
       (put-att current-table fname 'results nres)
       (put-att current-table fname 'body (cons 'progn body))
       (put-att current-table fname 'environment
          (if (eq current-table main-table)
	      'main
	      (list current-table main-table)))
       (process-args current-table fname)
       (if (eq current-table main-table)
	   (push-end fname to-compile))   ; must be used.
       fname))
    

;; Macros are essentially the same as functions except they dont have that
;; funky number sitting around after the parameters.  They are known only at
;; compile time so generate no code.

(defun def-ps-mac (fm)
   (let* ((macname (car fm))
	  (temp-name (intern (concat (symbol-name (car fm)) "--MACRO"))))
     (new-name macname)
     (define-name current-table macname 'user-macro)
     (eval `(defmacro ,temp-name ,@(cdr fm)))
     (put-att current-table macname 'new-name temp-name)
     macname))
   


;;  Here are the ps defvars.  Retain the fact that it's a variable (should
;;  watch for name conflicts, but I'm not going to worry right now).
;;  Save the initial value for generating initialization code.

(defun def-ps-var (fm)
   (let* ((vname (car fm))
	  (vvalue (cadr fm)))
     (new-name vname)
     (define-name current-table vname 'user-var)
     (put-att current-table vname 'environment
	      (if (eq current-table main-table)
		  'main
		  (list current-table main-table)))
     (put-att current-table vname 'init (or vvalue 0))
     (if (eq current-table main-table)
	 (push-end vname to-init))
     vname))

;;  defconst is slightly stranger than the others.  The value is evalled in
;;  the lisp world.  This means that one constant cant refer to another
;;  in the initializer.  This could be fixed using progv, but not right now.
;;  Code goes straight inline.

(defun def-ps-constant (fm)
   (let* ((cname (car fm))
	  (cur-consts (gethash '*****constants***** current-table))
	  (cvalue (eval `(let ,cur-consts ,(cadr fm)))))
     (new-name cname)
     (define-name current-table cname 'user-const)
     (put-att current-table cname 'value cvalue)
     (push (list cname `',cvalue)
		 (gethash '*****constants***** current-table))
     cname))

;;;  Here new libraries are loaded in.

(defun ps-load-library (libname)
  (let* ((current-table (make-hash-table))
	 (current-env (list current-table)))
    (push current-table env-list)
    (interpret-file libname)))

;;  This actually emits code for function definition.  This code takes
;;  place during initialization.  All that is generated is the body
;;  in braces to create a postscript code constant and the def which
;;  places the definition under the appropriate name in the dictionary.

(defun compile-function (fname)
  (let*
      ((all-vars (att main-table fname 'vars))
       (nres (att main-table fname 'results))
       (body (att main-table fname 'body))
       (env-att (att main-table fname 'environment))
       (current-env (if (eq env-att 'main) main-env env-att))
       (current-table (car current-env))
       (lexical-vars nil)
       (current-frame fname)
       (lexicals-here nil)
       (current-fn fname)
       real-nres)
    (emit `',fname)        ; emits /name
    (emit '\{)             ; start code object
    (emit `(alloc-frame ,current-frame))
    (emit 'begin)          ; enter new environment
    (for (:in arg (reverse all-vars))
                      ; bind parameters into local dict
	 (:do
	  (bind-it arg)))
    (setf real-nres (ps-compile body)) ; compile the body
    (wrap-frame)
    (when (/= nres real-nres)          ; make sure all's well
	      (ps-error "Wrong number of arguments returned" fname))
    (emit '\})
    (emit 'def)
    ))

;; Emit the initialization code for a global variable.  Just throw the initial
;; value into the dictionary

(defun compile-init-var (var)
  (let*  
      ((env-att (att main-table var 'environment))
       (current-env (if (eq env-att 'main) main-env env-att))
       (init (att main-table var 'init)))
    (compile-q var)
    (compile-1 init)
    (emit 'def)))

;; To recursively load a file, just reinvoke the interpreter

(defun ps-load (file &key filter)
    (interpret-file file :filter filter))

(defun wrap-frame ()
  (emit 'end)
  (put-att frame-table current-frame 'size
	   (length lexicals-here)))

