; Every Int recursion obligation the recursion check sends to its decision
; procedure while checking test_workspace (the only library in
; bosatsu_libs.json), one per line, without repeats. IntObligationCorpusTest
; replays them. Recorded with `tool test` on test_workspace, as test_cli.sh
; runs it, logging each (path facts, goal) pair TypedExprRecursionCheck.prove
; receives in this format.
(obligation (vars (candidate_1 Int) (rem_0 Int) (x_2 Int)) (facts (> rem_0 0) (< candidate_1 x_2) (not (= (ite (= candidate_1 0) x_2 (ite (> candidate_1 0) (mod x_2 candidate_1) (- 0 (mod (- 0 x_2) (- 0 candidate_1))))) 0)) (not (= (ite (= candidate_1 0) x_2 (ite (> candidate_1 0) (mod x_2 candidate_1) (- 0 (mod (- 0 x_2) (- 0 candidate_1))))) 0))) (goal (< (- rem_0 1) rem_0)))
(obligation (vars (candidate_1 Int) (rem_0 Int) (x_2 Int)) (facts (> rem_0 0) (< candidate_1 x_2) (not (= (ite (= candidate_1 0) x_2 (ite (> candidate_1 0) (mod x_2 candidate_1) (- 0 (mod (- 0 x_2) (- 0 candidate_1))))) 0)) (not (= (ite (= candidate_1 0) x_2 (ite (> candidate_1 0) (mod x_2 candidate_1) (- 0 (mod (- 0 x_2) (- 0 candidate_1))))) 0))) (goal (>= (- rem_0 1) 0)))
(obligation (vars (cnt_2 Int) (rem_0 Int) (total_3 Int)) (facts (> rem_0 0) (< cnt_2 total_3)) (goal (< (- rem_0 1) rem_0)))
(obligation (vars (cnt_2 Int) (rem_0 Int) (total_3 Int)) (facts (> rem_0 0) (< cnt_2 total_3)) (goal (>= (- rem_0 1) 0)))
(obligation (vars (curr_0 Int)) (facts (> curr_0 0)) (goal (< (div curr_0 10) curr_0)))
(obligation (vars (curr_0 Int)) (facts (> curr_0 0)) (goal (>= (div curr_0 10) 0)))
(obligation (vars (current_1 Int) (fuel_0 Int) (next_factor_2 Int)) (facts (> fuel_0 0) (not (= (ite (= next_factor_2 0) 0 (ite (> next_factor_2 0) (div current_1 next_factor_2) (div (- 0 current_1) (- 0 next_factor_2)))) 1))) (goal (< (- fuel_0 1) fuel_0)))
(obligation (vars (current_1 Int) (fuel_0 Int) (next_factor_2 Int)) (facts (> fuel_0 0) (not (= (ite (= next_factor_2 0) 0 (ite (> next_factor_2 0) (div current_1 next_factor_2) (div (- 0 current_1) (- 0 next_factor_2)))) 1))) (goal (>= (- fuel_0 1) 0)))
(obligation (vars (even_1 Bool) (n_0 Int)) (facts (not (or (< n_0 0) (= n_0 0))) (not (or (< n_0 0) (= n_0 0))) (not even_1) (not even_1)) (goal (< (div n_0 2) n_0)))
(obligation (vars (even_1 Bool) (n_0 Int)) (facts (not (or (< n_0 0) (= n_0 0))) (not (or (< n_0 0) (= n_0 0))) (not even_1) (not even_1)) (goal (>= (div n_0 2) 0)))
(obligation (vars (even_1 Bool) (n_0 Int)) (facts (not (or (< n_0 0) (= n_0 0))) (not (or (< n_0 0) (= n_0 0))) even_1) (goal (< (div (- n_0 1) 2) n_0)))
(obligation (vars (even_1 Bool) (n_0 Int)) (facts (not (or (< n_0 0) (= n_0 0))) (not (or (< n_0 0) (= n_0 0))) even_1) (goal (>= (div (- n_0 1) 2) 0)))
(obligation (vars (i_0 Int)) (facts (> i_0 0)) (goal (< (- i_0 1) i_0)))
(obligation (vars (i_0 Int)) (facts (> i_0 0)) (goal (>= (- i_0 1) 0)))
(obligation (vars (i_0 Int)) (facts (not (or (< i_0 0) (= i_0 0))) (not (or (< i_0 0) (= i_0 0)))) (goal (< (- i_0 1) i_0)))
(obligation (vars (i_0 Int)) (facts (not (or (< i_0 0) (= i_0 0))) (not (or (< i_0 0) (= i_0 0)))) (goal (< (div i_0 2) i_0)))
(obligation (vars (i_0 Int)) (facts (not (or (< i_0 0) (= i_0 0))) (not (or (< i_0 0) (= i_0 0)))) (goal (>= (- i_0 1) 0)))
(obligation (vars (i_0 Int)) (facts (not (or (< i_0 0) (= i_0 0))) (not (or (< i_0 0) (= i_0 0)))) (goal (>= (div i_0 2) 0)))
(obligation (vars (idx_0 Int) (s_3 Int)) (facts (not (< idx_0 0)) (not (= idx_0 0)) (and (not (< idx_0 0)) (not (= idx_0 0))) (< idx_0 s_3)) (goal (< (- idx_0 1) idx_0)))
(obligation (vars (idx_0 Int) (s_3 Int)) (facts (not (< idx_0 0)) (not (= idx_0 0)) (and (not (< idx_0 0)) (not (= idx_0 0))) (< idx_0 s_3)) (goal (>= (- idx_0 1) 0)))
(obligation (vars (idx_0 Int) (s_3 Int)) (facts (not (< idx_0 0)) (not (= idx_0 0)) (not (< idx_0 s_3)) (and (not (< idx_0 0)) (not (= idx_0 0))) (> s_3 0)) (goal (< (- idx_0 s_3) idx_0)))
(obligation (vars (idx_0 Int) (s_3 Int)) (facts (not (< idx_0 0)) (not (= idx_0 0)) (not (< idx_0 s_3)) (and (not (< idx_0 0)) (not (= idx_0 0))) (> s_3 0)) (goal (>= (- idx_0 s_3) 0)))
(obligation (vars (idx_0 Int)) (facts (not (< idx_0 0)) (not (= idx_0 0)) (and (not (< idx_0 0)) (not (= idx_0 0))) (or (= idx_0 1) (> idx_0 1))) (goal (< (- idx_0 1) idx_0)))
(obligation (vars (idx_0 Int)) (facts (not (< idx_0 0)) (not (= idx_0 0)) (and (not (< idx_0 0)) (not (= idx_0 0))) (or (= idx_0 1) (> idx_0 1))) (goal (>= (- idx_0 1) 0)))
(obligation (vars (n_2 Int) (rem_0 Int) (trial_1 Int)) (facts (not (= (ite (= trial_1 0) n_2 (ite (> trial_1 0) (mod n_2 trial_1) (- 0 (mod (- 0 n_2) (- 0 trial_1))))) 0)) (not (= (ite (= trial_1 0) n_2 (ite (> trial_1 0) (mod n_2 trial_1) (- 0 (mod (- 0 n_2) (- 0 trial_1))))) 0)) (> rem_0 1)) (goal (< (- rem_0 1) rem_0)))
(obligation (vars (n_2 Int) (rem_0 Int) (trial_1 Int)) (facts (not (= (ite (= trial_1 0) n_2 (ite (> trial_1 0) (mod n_2 trial_1) (- 0 (mod (- 0 n_2) (- 0 trial_1))))) 0)) (not (= (ite (= trial_1 0) n_2 (ite (> trial_1 0) (mod n_2 trial_1) (- 0 (mod (- 0 n_2) (- 0 trial_1))))) 0)) (> rem_0 1)) (goal (>= (- rem_0 1) 0)))
(obligation (vars (rem_0 Int)) (facts (> rem_0 0)) (goal (< (- rem_0 1) rem_0)))
(obligation (vars (rem_0 Int)) (facts (> rem_0 0)) (goal (>= (- rem_0 1) 0)))
