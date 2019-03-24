```
Exercise 5.19.  Alyssa P. Hacker wants a breakpoint feature in the simulator to help her debug her machine designs. You have been hired to install this feature for her. She wants to be able to specify a place in the controller sequence where the simulator will stop and allow her to examine the state of the machine. You are to implement a procedure

(set-breakpoint <machine> <label> <n>)

that sets a breakpoint just before the nth instruction after the given label. For example,

(set-breakpoint gcd-machine 'test-b 4)

installs a breakpoint in gcd-machine just before the assignment to register a. When the simulator reaches the breakpoint it should print the label and the offset of the breakpoint and stop executing instructions. Alyssa can then use get-register-contents and set-register-contents! to manipulate the state of the simulated machine. She should then be able to continue execution by saying

(proceed-machine <machine>)

She should also be able to remove a specific breakpoint by means of

(cancel-breakpoint <machine> <label> <n>)

or to remove all breakpoints by means of

(cancel-all-breakpoints <machine>)
```

Modify *extract-labels* to attach location information to every instruction, and make the machine to save labes information and breakpoints. Check every instruction with the breakpoints, stop execution when matched.

```
(define (print x)
  (begin
    (display x)
    (newline)))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))


(define (extract-labels text receive)
  (define (extract-labels-info label count text receive)
    (if (null? text)
        (receive '() '())
        (extract-labels-info
         (if (symbol? (car text))
             (car text)
             label)
         (if (symbol? (car text))
             count
             (+ count 1))
         (cdr text)
         (lambda (insts labels)
           (let ((next-inst (car text)))
             (if (symbol? next-inst)
                 (begin
                   (set! label next-inst)
                   (set! count 0)
                   (receive insts
                            (cons (make-label-entry next-inst
                                                    insts)
                                  labels)))
                 (begin
                   (set! count (+ count 1))
                   (receive (cons ((make-instruction next-inst) label count)
                                  insts)
                            labels)
                   )))))))
  (extract-labels-info '() 0 text receive))


(define (make-instruction text)
  (lambda (label line)
    (list text '() (list label line))))

(define (update-insts! insts labels machine)
  (let ((pc (get-register machine 'pc))
        (flag (get-register machine 'flag))
        (stack (machine 'stack))
        (ops (machine 'operations)))
    ((machine 'set-labels) labels)
    (for-each
     (lambda (inst)
       (set-instruction-execution-proc! 
        inst
        (make-execution-procedure
         (instruction-text inst) labels machine
         pc flag stack ops)))
     insts)))

(define (instruction-execution-proc inst)
  (cadr inst))

(define (set-instruction-execution-proc! inst proc)
    (set-car! (cdr inst) proc))

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '())
        (labels '())
        (breakpoints '()))
    (let ((the-ops
           (list (list 'initialize-stack
                       (lambda () (stack 'initialize)))
                 ;;**next for monitored stack (as in section 5.2.4)
                 ;;  -- comment out if not wanted
                 (list 'print-stack-statistics
                       (lambda () (stack 'print-statistics)))))
          (register-table
           (list (list 'pc pc) (list 'flag flag))))
      (define (allocate-register name)
        (if (assoc name register-table)
            (error "Multiply defined register: " name)
            (set! register-table
                  (cons (list name (make-register name))
                        register-table)))
        'register-allocated)
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
              (cadr val)
              (error "Unknown register:" name))))
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (let ((matched-breakpoints (filter
                                          (lambda (breakpoint) (eq-breakpoint? breakpoint (inst-location (car insts))))
                                          breakpoints)))
                (if (null? matched-breakpoints)
                    (proceed-execute)
                    (begin
                      (display (list 'label: (caar matched-breakpoints) 'offset: (cadar matched-breakpoints)))
                      (newline)))))))
      (define (proceed-execute)
        (let ((insts (get-contents pc)))
          ((instruction-execution-proc (car insts)))
          (execute)))
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
              ((eq? message 'install-instruction-sequence)
               (lambda (seq) (set! the-instruction-sequence seq)))
              ((eq? message 'allocate-register) allocate-register)
              ((eq? message 'get-register) lookup-register)
              ((eq? message 'install-operations)
               (lambda (ops) (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
              ((eq? message 'set-labels) (lambda (lab) (set! labels lab)))
              ((eq? message 'set-breakpoint) (lambda (label n) (set! breakpoints (add-breakpoint label n labels breakpoints))))
              ((eq? message 'print-breakpoints) (print breakpoints))
              ((eq? message 'cancel-breakpoint)
               (lambda (label n)
                 (set! breakpoints
                       (filter (lambda (breakpoint)
                                 (not (eq-breakpoint? breakpoint (list label n))))
                               breakpoints))))
              ((eq? message 'cancel-all-breakpoints) (set! breakpoints '()))
              ((eq? message 'proceed-machine) (proceed-execute))
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

(define (proceed-machine machine)
  (machine 'proceed-machine))

(define (inst-location inst)
  (caddr inst))

(define (cancel-breakpoint machine label n)
  ((machine 'cancel-breakpoint) label n))

(define (cancel-all-breakpoints machine)
  (machine 'cancel-all-breakpoints))

(define (eq-breakpoint? bp1 bp2)
  (and (eq? (car bp1) (car bp2))
       (= (cadr bp1) (cadr bp2))))

(define (add-breakpoint label n labels breakpoints)
  (let ((val (assoc label labels)))
    (if val
        (cons (list label n) breakpoints)
        (error "unknown lable" lable))))

(define (set-breakpoint machine label n)
  ((machine 'set-breakpoint) label n))

```