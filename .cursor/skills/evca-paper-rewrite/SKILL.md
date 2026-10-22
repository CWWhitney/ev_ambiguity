# EVCA Paper Rewrite Skill

Purpose: Provide one stable process for all EVCA manuscript revisions (v4, v5, and later), so definitions, claims, writing style, and reviewer-response logic stay consistent across drafts.

## Canonical Source Bundle (keep together)

Treat the following as one linked source bundle for every rewrite pass:
- Mark legitimacy critique:
  - ../potential_co_author_feedback/Mark_Strong/Mark_feedback_v1
- Keith review set:
  - ../potential_co_author_feedback/Keith_Shepherd/EVCA_IS_NOT_NEW.txt
  - ../potential_co_author_feedback/Keith_Shepherd/INFORMS_Review.txt
  - ../potential_co_author_feedback/Keith_Shepherd/ChatGPT_Review_KS.txt
  - ../potential_co_author_feedback/Keith_Shepherd/Claude_Review_KS.txt
- Resolved short-manuscript implementation:
  - ../manuscript_short/orl_short_communication.Rmd
- Repository memory log of accepted resolutions:
  - /memories/repo/evca_review_findings.md

If any draft conflicts with this bundle, revise the draft. Do not reinterpret ad hoc.

Use this skill when:
- Revising manuscript framing, methods, discussion, or conclusion
- Responding to reviewer or co-author critique on novelty, legitimacy, assumptions, or interpretation
- Porting improvements between short and long manuscripts
- Checking consistency between paper claims and package behavior

Do not use this skill for:
- Routine package-only maintenance unrelated to manuscript claims
- Frontend, app UI, or unrelated analytics tasks

## Canonical EVCA Framing

Use this wording logic consistently:
- EVCA is a decision-analytic diagnostic for the welfare cost of aggregation under contested causal ontologies.
- The core novelty is explicit utility-bridging plus exclusion-cost diagnostics in structurally incommensurable settings.
- Do not claim EVCA replaces EVPI or BMA.
- Do not claim non-overlapping internal variables alone make EVPI mathematically undefined.
- Distinguish two readings of model weights p(Mk):
  - Epistemic reading: p(Mk) as credence over rival world models.
  - Evaluative reading: p(Mk) as normative standing assigned to stakeholder accounts.
- For stakeholder-ontology applications, default to evaluative reading unless explicitly justified otherwise.

## Feedback-to-Rule Mapping (required)

Map critique directly to manuscript rules:
1. Mark feedback rule (legitimacy):
  - Never present a headline EVCA number without declaring who set model weights and scaling.
  - Always frame normalization and weighting as political/normative choices.
2. Keith feedback rule (novelty discipline):
  - Never claim that EVPI/BMA are categorically undefined.
  - State that once a bridge exists, algebra resembles EVPI; novelty is explicit bridge construction and exclusion diagnostics in structurally incommensurable settings.
3. Keith feedback rule (assumption exposure):
  - State min-max identity implications when min-max on realized action set is used.
  - State scope conditions including IIA sensitivity and deterministic-action restriction.
4. Short-manuscript rule (already resolved):
  - Keep evaluative p(Mk) framing consistent unless explicitly switching framing and justifying the switch.
  - Keep exclusion-cost interpretation separate from aggregate EVCA interpretation.

## Non-Negotiable Clarifications

Always enforce the following points in manuscript prose:
1. Utility bridge Bk is normative, not discovered from data.
2. Min-max normalization imposes equal-stakes comparability across model ranges.
3. Results are conditional on chosen weights and normalization.
4. Exclusion cost is distinct from being outvoted.
5. Legitimacy depends on who set weights and scaling, and how this was justified.
6. Manuscript claims must align with the already-resolved short-manuscript language decisions.

## Claim-Safety Rules

Avoid these risky phrases unless heavily qualified:
- correct model
- true model
- no prior reason to prefer any model
- perfect structural knowledge (in purely evaluative framing)
- EVCA is entirely new
- existing methods are not defined
- EVCA proves whose model is right

Preferred replacements:
- stakeholder model standing
- declared weighting rule
- aggregation rule
- disaggregation benchmark
- conditional on normalization and weighting choices
- legitimacy-conditional welfare diagnostic

## v4/v5 Rewrite Workflow

Step 1. Framing lock
- State whether the paper uses epistemic or evaluative p(Mk).
- If evaluative, remove language that implies evidence can reveal the one true stakeholder model.

Step 2. Novelty reposition
- Recast novelty as process transparency and explicit aggregation-cost accounting.
- Position EVCA relative to EVPI, BMA, robust decision traditions, and social-choice aggregation.

Step 2b. Critique-port sweep from short manuscript
- Port all resolved framing and interpretation fixes from ../manuscript_short/orl_short_communication.Rmd into the long manuscript before adding new prose.
- Do not keep parallel contradictory wording between short and long versions.

Step 3. Methods integrity
- Ensure formulas and text match the chosen framing.
- If min-max on realized action set D is used, explicitly discuss implications and limits.
- Keep exclusion-cost definition consistent with package implementation.

Step 4. Legitimacy protocol section
- Add explicit reporting requirements:
  1) who chose weights,
  2) why those weights,
  3) which normalization/anchors,
  4) sensitivity envelope,
  5) exclusion consequences by stakeholder.

Step 4b. Citation safety check
- Ensure decision-analytic positioning cites the established anchors used in the short manuscript and repo memory notes.
- Do not claim novelty where the cited literature already covers the operation.

Step 5. Cross-document sync
- Propagate core framing to all active manuscript files and package vignette text that states interpretation.
- Do not leave contradictory wording between manuscripts.

Step 6. Final consistency audit
- Run targeted search for risky terms and replace as needed.
- Confirm abstract, intro, methods, discussion, and conclusion all tell the same story.

## Writing Style Standard

Target style:
- Precise, modest, and explicit about assumptions.
- Avoid inflated novelty language.
- Keep normative choices visible, not hidden in technical prose.
- Use plain language to explain implications for deliberation and policy legitimacy.
- Keep novelty claims narrow and operational.

Structure guidance:
- Abstract: problem, scoped contribution, conditional interpretation, practical diagnostic value.
- Introduction: gap statement with careful boundary conditions.
- Methods: definitions first, assumptions explicit, then computation.
- Results: separate numerical findings from interpretation and legitimacy implications.
- Discussion: relation to prior frameworks, what is and is not claimed.
- Conclusion: practical contribution and limits, no overclaim.

## Ready-to-Use Review Checklist

Before marking a revision complete, confirm:
- Terminology consistency across all sections
- No contradiction between framing and code interpretation
- Legitimacy protocol included
- Sensitivity analysis described as normative-scenario exploration, not truth discovery
- Exclusion-cost interpretation retained and clearly separated from EVCA headline value
- Mark-critique condition satisfied: EVCA value is not presented without authorship of weights/scaling
- Keith-critique condition satisfied: no overclaim that prior methods are globally invalid
- Short-manuscript parity satisfied on framing and caveats

## Suggested Search Patterns

Use these repository searches during audits:
- correct model|true model|plausible|prior reason|perfect structural knowledge
- not defined|new quantity|replaces EVPI|replaces BMA
- min-max|normalization|anchor|independence of irrelevant alternatives
- exclusion cost|outvoted|unmodeled
- legitimacy|standing|political choice|normalization choice
- EVPI|BMA|undefined|new quantity

## Deliverable Standard for Each Revision Pass

Every revision pass should produce:
1. Short change summary by section
2. List of framing risks removed
3. List of assumptions made explicit
4. Remaining open issues that need author-level decisions

## Long-Manuscript Migration Pack (v3 -> v4/v5)

Use this pack when updating ../manuscript/paper_v3.Rmd or creating a new v4 file:
1. Framing migration
- Replace epistemic truth-language with evaluative standing-language where short manuscript already resolved this.
2. Methods migration
- Port min-max identity interpretation and conditionality statements from short manuscript treatment.
3. Scope migration
- Port IIA caveat and deterministic-vs-mixed strategy caveat.
4. Discussion migration
- Port disciplined novelty language and relationship-to-prior-frameworks language.
5. Conclusion migration
- Port conservative contribution claim style: explicit combination of established components plus practical diagnostic framing.

Output required after migration:
1. Section-by-section diff summary
2. List of unresolved wording conflicts between short and long manuscripts
3. Final pass of risky-term search with zero high-risk hits

This skill should be treated as the default protocol for all manuscript work toward v4, v5, and submission-quality drafts.
