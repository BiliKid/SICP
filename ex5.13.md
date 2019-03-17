```
Exercise 5.13.  Modify the simulator so that it uses the controller sequence to determine what registers the machine has rather than requiring a list of registers as an argument to make-machine. Instead of pre-allocating the registers in make-machine, you can allocate them one at a time when they are first seen during assembly of the instructions.
```
modify the subprocedure of assemble--*update-insts!*

```
(define (update-insts! insts labels machine)
  (let ((pc (get-register machine 'pc))
        (flag (get-register machine 'flag))
        (stack (machine 'stack))
        (ops (machine 'operations)))
    (for-each
     (lambda (inst)
       (let ((rest (cdr inst)))
         (find-reg rest machine)
       (set-instruction-execution-proc! 
        inst
        (make-execution-procedure
         (instruction-text inst) labels machine
         pc flag stack ops))))
     insts)))

(define (find-reg inst machine)
  (if (null? inst)
      'done
      (begin
       (cond
        ((symbol? (car inst))
         ((machine 'allocate-register) (car inst)))
        ((register-exp? (car inst))
         ((machine 'allocate-register) (cadar inst))))
       (find-reg (cdr inst) machine))))
       
;modify the allocate-register of make-new-machine
(define (allocate-register name)
        (if (not (assoc name register-table))         ;allocate when not find
            (set! register-table
                  (cons (list name (make-register name))
                        register-table)))
        'register-allocated)

```

