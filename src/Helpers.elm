module Helpers exposing (flip)


flip : (b -> a -> c) -> a -> b -> c
flip fn a b =
    fn b a
