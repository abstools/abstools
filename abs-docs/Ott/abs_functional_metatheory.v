Require Import abs_functional.
Require Import List.
Require Import ssreflect.

Definition subG (G5 G' : G) : Prop :=
  forall (key : string) (elt : ctxv), 
    Map.find key G5 = Some elt ->
    Map.find key G' = Some elt.

Lemma type_preservation_var_aux : forall (G5 : G) (tsubst5 : tsubst) (x5 : x) (t5 : t) (A5 : A),
  t_e G5 (e_var x5) A5 ->
  t_tsubst G5 tsubst5 ->
  Map.find x5 tsubst5 = Some t5 ->
  t_e G5 (e_term t5) A5.
Proof.
move => G5 tsubst5 x5 t5 A5 H_t_e H_ts H_find.
inversion H_ts; subst.
apply (H _ _ A5) in H_find => //.
by inversion H_t_e; subst.
Qed.

Lemma type_preservation_var : forall (G5 : G) (tsubst5 : tsubst) (x5 : x) (t5 : t) (A5 : A),
  t_e G5 (e_var x5) A5 -> 
  t_tsubst G5 tsubst5 ->
  Map.find (elt:=t) x5 tsubst5 = Some t5 ->
  exists G' : G, subG G5 G' /\ t_tsubst G' tsubst5 /\ t_e G' (e_term t5) A5.
Proof.
move => G5 tsubst5 x5 t5 A5 H_t_e H_ts H_find.
exists G5; split => //; split => //.
move: H_t_e H_ts H_find.
exact: type_preservation_var_aux.
Qed.

Lemma type_preservation_cons_aux : forall (e5 : e) (e_list : list e) (G5 : G) (Co6 : Co) (A5 : A),
  In e5 e_list ->
  t_e G5 (e_co_param Co6 e_list) A5 ->
  exists A', t_e G5 e5 A'.
Proof.
Admitted.
      
Lemma type_preservation : forall (G5 : G) (tsubst5 : tsubst), 
  t_tsubst G5 tsubst5 ->
  forall (Flist : list F) (e5 : e) (A5 : A) (tsubst' : tsubst) (e' : e),
    t_e G5 e5 A5 ->
    red_tsubst_e Flist tsubst5 e5 tsubst' e' ->
    exists G', subG G5 G' /\ t_tsubst G' tsubst' /\ t_e G' e' A5.
Proof.
move => G5 tsubst5 H_wt Flist e5 A5 tsubst' e' H_t_e H_red.
move: H_red G5 H_wt A5 H_t_e.
elim.
- (* red_var *)
  move => Flist' tsubst6 x5 t5 H_find G5 H_ts A5 H_t_e.
  move: H_t_e H_ts H_find.
  exact: type_preservation_var.
- (* red_cons *)
  move => e'_list e_list F_list tsubst6 Co6 e6 tsubst7 e7 H_red IH.
  move => G5 H_wt A5 H_t_e.
  have IH' := IH _ H_wt. 
  have H_ex := type_preservation_cons_aux.
  
