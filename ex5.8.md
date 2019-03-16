```
Exercise 5.8.  The following register-machine code is ambiguous, because the label here is defined more than once:

start
  (goto (label here))
here
  (assign a (const 3))
  (goto (label there))
here
  (assign a (const 4))
  (goto (label there))
there

With the simulator as written, what will the contents of register a be when control reaches there? Modify the extract-labels procedure so that the assembler will signal an error if the same label name is used to indicate two different locations.
```

To check whether a label is duplicate, just insert a lookup procedure before new label add into labels table.

```
(define (extract-labels text receive)
  (if (null? text)
      (receive '() '())
      (extract-labels (cdr text)
       (lambda (insts labels)
         (let ((next-inst (car text)))
           (if (symbol? next-inst)
               (let ((val (assoc next-inst labels)))
                 (if val
                     (error "Duplicate label -- ERROR" next-inst)
                     (receive insts
                              (cons (make-label-entry next-inst
                                                      insts)
                                    labels))))
               (receive (cons (make-instruction next-inst)
                              insts)
                        labels)))))))
```