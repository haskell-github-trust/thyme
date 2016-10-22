#define LENS(S,F,A) {-# INLINE _/**/F #-}; _/**/F :: Lens' S A; _/**/F = lens F $ \ S {..} F/**/_ -> S {F = F/**/_, ..}
