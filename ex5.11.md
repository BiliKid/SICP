```
Exercise 5.11.  When we introduced save and restore in section 5.1.4, we didn't specify what would happen if you tried to restore a register that was not the last one saved, as in the sequence

(save y)
(save x)
(restore y)

There are several reasonable possibilities for the meaning of restore:

a.  (restore y) puts into y the last value saved on the stack, regardless of what register that value came from. This is the way our simulator behaves. Show how to take advantage of this behavior to eliminate one instruction from the Fibonacci machine of section 5.1.4 (figure 5.12).

b.  (restore y) puts into y the last value saved on the stack, but only if that value was saved from y; otherwise, it signals an error. Modify the simulator to behave this way. You will have to change save to put the register name on the stack along with the value.

c.  (restore y) puts into y the last value saved from y regardless of what other registers were saved after y and not restored. Modify the simulator to behave this way. You will have to associate a separate stack with each register. You should make the initialize-stack operation initialize all the register stacks.
```

a. 
```
(controller
   (assign continue (label fib-done))
 fib-loop
   (test (op <) (reg n) (const 2))
   (branch (label immediate-answer))
   ;; set up to compute Fib(n - 1)
   (save continue)
   (assign continue (label afterfib-n-1))
   (save n)                           ; save old value of n
   (assign n (op -) (reg n) (const 1)); clobber n to n - 1
   (goto (label fib-loop))            ; perform recursive call
 afterfib-n-1                         ; upon return, val contains Fib(n - 1)
   (restore n)
   ;(restore continue)
   ;; set up to compute Fib(n - 2)
   (assign n (op -) (reg n) (const 2))
   ;(save continue)
   (assign continue (label afterfib-n-2))
   (save val)                         ; save Fib(n - 1)
   (goto (label fib-loop))
 afterfib-n-2                         ; upon return, val contains Fib(n - 2)
   ;(assign n (reg val))               ; n now contains Fib(n - 2)
   ;(restore val)                      ; val now contains Fib(n - 1)
   (restore n)                         ; val now contains Fib(n - 1) and val contain Fib(n - 2)
   (restore continue)
   (assign val                        ;  Fib(n - 1) +  Fib(n - 2)
           (op +) (reg val) (reg n)) 
   (goto (reg continue))              ; return to caller, answer is in val
 immediate-answer
   (assign val (reg n))               ; base case:  Fib(n) = n
   (goto (reg continue))
 fib-done)
```

b.  change *make-save* and *mak-restore*. The disadvantage of this make-store implement is that stack haved been popped.

```
(define (make-save inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (push stack (cons (stack-inst-reg-name inst) (get-contents reg)))
      (advance-pc pc))))

(define (make-restore inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (let ((pop-val (pop stack)))
        (if (eq? (stack-inst-reg-name inst) (car pop-val))
            (begin
              (set-contents! reg (pop stack (stack-inst-reg-name inst)))
              (advance-pc pc))
            (error "reg-name isn't correct -- ERROR" (stack-inst-reg-name inst)))))))
```

c.  change *initialize-stack* operation to a procedure of *make-new-machine*, run ```(machine 'initialize-stack)``` every time when start, add some print information to debug, modify the related interface.

```
; use the origin make-stack to modify
(define (make-stack)
  (let ((s '()))
    (define (push reg-name val)
      (let ((reg (assoc reg-name s)))
        (if reg
            (set-cdr! reg (cons val (cdr reg)))
            (error "register is not exist -- PUSH STACK" reg-name " " s))))
    (define (pop reg-name)
      (let ((reg (assoc reg-name s)))
        (if reg
            (if (null? (cdr reg))
                (error "register's stack is emtpy -- POP STACK" reg-name)
                (let ((top (cadr reg)))
                  (set-cdr! reg (cddr reg))
                  top)))))
    (define (initialize register-table)
      (for-each (lambda (register) (set! s (cons (list (car register) '()) s))) register-table)
      'done)
    (define (print)
      (display s)
      (newline))
    (define (dispatch message)
      (cond ((eq? message 'push) push)
            ((eq? message 'pop) pop)
            ((eq? message 'initialize) initialize)
            ((eq? message 'print) (print))
            (else (error "Unknown request -- STACK"
                         message))))
    dispatch))

;change make-save and make-restore interface to pass register-name
(define (make-save inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (push stack (stack-inst-reg-name inst) (get-contents reg))
      (advance-pc pc))))

(define (make-restore inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (set-contents! reg (pop stack (stack-inst-reg-name inst)))    
      (advance-pc pc))))

;change pop and push
(define (pop stack reg)
  ((stack 'pop) reg))

(define (push stack reg value)
  ((stack 'push) reg value))

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '()))
    (let ((register-table
           (list (list 'pc pc) (list 'flag flag)))
          (the-ops
           (list
;            (list 'initialize-stack
;                       (lambda () ((stack 'initialize) register-table)))   
                 ;;**next for monitored stack (as in section 5.2.4)
                 ;;  -- comment out if not wanted
                 (list 'print-stack-statistics
                       (lambda () (stack 'print-statistics))))))
      (define (initialize-stack)              ;change the operation to procedure
        ((stack 'initialize) register-table))
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
              (begin
                ((instruction-execution-proc (car insts)))
                (execute)))))
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
              ((eq? message 'print-stack) (stack 'print))
              ((eq? message 'operations) the-ops)
              ((eq? message 'print-register-table) (display register-table) (newline))
              ((eq? message 'initialize-stack) (initialize-stack))
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

;add a test-machine
(define test-machine
  (make-machine
   '(x y)
   '()
   '(
     (assign x (const 1))
     (save x)
     (assign y (const 2))
     (save y)
     (assign x (const 2))
     (save x)
(restore x))))
(test-machine 'initialize-stack)
(test-machine 'print-stack)
(test-machine 'print-register-table)
(start test-machine)
(test-machine 'print-stack)
(get-register-contents test-machine 'x)
```