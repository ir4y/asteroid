-module(asteroid_handler).

-callback handle(bitstring(),list()) 
  -> bitstring() | {fun(),bitstring()}.

-callback is_periodical(bitstring()) -> boolean().
