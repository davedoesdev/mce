(print (get-config "foo"))
; test with e.g.
; ./scm/expand < ./examples/test-config.scm | ./scm/scan | ./scm/mce --config "foo=$(echo '(display-binary (save "bar"))' | ./scm/expand | ./scm/scan | ./scm/mce)"
