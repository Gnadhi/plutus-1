---
title: VerifiedCompilation.UCSE
layout: page
---

# Common Subexpression Elimination Translation Phase
```
module VerifiedCompilation.UCSE where

```
## Imports

```
open import Untyped.Equality using (DecEq; _≟_; decPointwise)
open import VerifiedCompilation.UntypedViews using (Pred; isCase?; isApp?; isLambda?; isForce?; isBuiltin?; isConstr?; isDelay?; isTerm?; allTerms?; iscase; isapp; islambda; isforce; isbuiltin; isconstr; isterm; allterms; isdelay)
open import VerifiedCompilation.UntypedTranslation using (Translation; translation?; Relation)
open import Relation.Nullary using (_×-dec_)
open import Data.Product using (_,_)
import Relation.Binary as Binary using (Decidable)
open import Relation.Nullary using (Dec; yes; no; ¬_)
open import Untyped using (_⊢; case; builtin; _·_; force; `; ƛ; delay; con; constr; error)
import Relation.Binary.PropositionalEquality as Eq
open Eq using (_≡_; refl)
open import Data.Empty using (⊥)
open import Agda.Builtin.Maybe using (Maybe; just; nothing)
open import Untyped.RenamingSubstitution using (_[_])
open import Untyped.Purity using (Pure; isPure?)
open import VerifiedCompilation.Certificate using (ProofOrCE; ce; proof; pcePointwise; MatchOrCE; cseT)
```
## Translation Relation

This module is required to certify that an application of CSE doesn't break the
semantics; we are explicitly not evaluating whether the particular choice of
sub-expression was a "good" choice.

As such, this Translation Relation primarily checks that substituting the expression
back in would yield the original expression.

```
data UCSE : Relation where
  cse : {X : Set} {{ _ : DecEq X}} {x' : Maybe X ⊢} {x e : X ⊢}
    -- TODO: This should ensure that the term that is moved
    -- is still evaluated. The Haskell does this by never moving
    -- across ƛ , delay, or case.
    → Translation UCSE x (x' [ e ])
    → UCSE x ((ƛ x') · e)

UntypedCSE : {X : Set} {{_ : DecEq X}} → (ast : X ⊢) → (ast' : X ⊢) → Set₁
UntypedCSE = Translation UCSE

```

## Decision Procedure

```

isUntypedCSE? : {X : Set} {{_ : DecEq X}} → MatchOrCE (Translation UCSE {X})

{-# TERMINATING #-}
isUCSE? : {X : Set} {{_ : DecEq X}} → MatchOrCE (UCSE {X})
isUCSE? ast ast' with (isApp? (isLambda? isTerm?) isTerm?) ast'
... | no ¬match = ce (λ { (cse pt) → ¬match (isapp (islambda (isterm _)) (isterm _))}) cseT ast ast'
... | yes (isapp (islambda (isterm x')) (isterm e)) with (isUntypedCSE? ast (x' [ e ]))
...   | ce ¬p t b a = ce (λ { (cse pt) → ¬p pt}) t b a
...   | proof p = proof (cse p)
isUntypedCSE? = translation? cseT isUCSE?
```
