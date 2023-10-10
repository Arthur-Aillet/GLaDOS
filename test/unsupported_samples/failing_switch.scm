(define (switch x a b c d e f g h i j)
    (if (eq? x 0)
        a
        (if (eq? x 1)
            b
            (if (eq? x 2)
                c
                (if (eq? x 3)
                    d
                    (if (eq? x 4)
                        e
                        (if (eq? x 5)
                            f
                            (if (eq? x 6)
                                g
                                (if (eq? x 7)
                                    h
                                    (if (eq? x 8)
                                        i
                                        (if (eq? x 9)
                                            j
                                            error
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            )
        )
    )
)

(switch 100 10 11 12 13 14 15 16 17 18 19)
