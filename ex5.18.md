```
Exercise 5.18.  Modify the make-register procedure of section 5.2.1 so that registers can be traced. Registers should accept messages that turn tracing on and off. When a register is traced, assigning a value to the register should print the name of the register, the old contents of the register, and the new contents being assigned. Extend the interface to the machine model to permit you to turn tracing on and off for designated machine registers.
```

define a global variable *g-reg-trace* for all the registers to trace on or not, it's silly. You can also define a switch for every register when *make-register*, but it's tedious to trace on or off the register you want.

```
(define g-reg-trace 'off)

(define (make-register name)
  (let ((contents '*unassigned*))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
            ((eq? message 'set)
               (lambda (value) (begin (if (eq? g-reg-trace 'on)
                                     (display (list 'register-name: name 'old-value: contents 'new-value: value)))
                                     (newline)
                               (set! contents value))))
            (else
             (error "Unknown request -- REGISTER" message))))
    dispatch))

(set! g-reg-trace 'on)
(set! g-reg-trace 'off)

```