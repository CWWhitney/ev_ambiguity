# EVCA Long Manuscript Migration Plan (v3 -> v4)

Goal: Convert the long manuscript to the same resolved framing and claim discipline already implemented in the short manuscript, while preserving technical depth suitable for the full paper format.

## Definition of Done

A v4 manuscript is done when all conditions are met:
1. No unresolved epistemic-vs-evaluative contradiction in any section.
2. Mark legitimacy critique is fully operationalized through explicit reporting protocol.
3. Keith novelty critique is resolved through narrowed contribution claims and correct positioning.
4. Core caveats from the short manuscript are present (normalization conditionality, IIA sensitivity, action-space limits).
5. Abstract, methods, discussion, and conclusion tell one consistent story.

## Execution Sequence

## Phase 1: Framing Lock (Day 1)

Tasks:
1. Decide and state framing for p(Mk) in v4 front matter and methods preamble.
2. Replace truth-discovery language in EVCA and sensitivity sections with standing/weight language.
3. Add one explicit paragraph that separates epistemic and evaluative readings and states which one the manuscript uses.

Acceptance checks:
1. No high-risk terms remain without qualification.
2. Results interpretation does not imply empirical identification of one true stakeholder ontology.

## Phase 2: Methods Harmonization (Day 1-2)

Tasks:
1. Align long-manuscript EVCA definition language with short-manuscript resolved language.
2. Insert min-max identity interpretation when min-max on realized action set is retained.
3. Ensure exclusion-cost definition text matches package semantics and manuscript interpretation.
4. Replace hand-rolled snippets with package-grounded calls where feasible.

Acceptance checks:
1. Formula text and code interpretation are consistent.
2. Exclusion cost is clearly distinct from being outvoted.

## Phase 3: Legitimacy Protocol Integration (Day 2)

Tasks:
1. Add a dedicated subsection named Legitimacy and Accountability Protocol.
2. Require reporting of:
   - who chose weights,
   - why those weights,
   - chosen normalization/anchors,
   - sensitivity envelope,
   - stakeholder-specific exclusion consequences.
3. Update captions for headline result tables/figures to reflect conditional interpretation.

Acceptance checks:
1. No headline EVCA statement appears without the protocol context.
2. Mark critique is directly answered in the manuscript body, not only in caveats.

## Phase 4: Scope and Limits Port (Day 2-3)

Tasks:
1. Port IIA sensitivity caveat for min-max-on-D.
2. Port deterministic-vs-mixed strategy caveat.
3. Port boundary-case clarifications used in short manuscript where relevant.

Acceptance checks:
1. Limitations section contains concrete scope conditions, not generic disclaimers.
2. Reviewer can identify exactly where construct validity depends on assumptions.

## Phase 5: Discussion Repositioning (Day 3)

Tasks:
1. Reframe novelty as explicit aggregation audit under contested ontologies.
2. Position relationship to EVPI, BMA, robust decision traditions, and social-choice aggregation with disciplined wording.
3. Remove any residual overclaim language.

Acceptance checks:
1. Contribution claim is modest, precise, and defensible.
2. Discussion does not contradict methods framing.

## Phase 6: Abstract and Conclusion Rewrite (Day 3)

Tasks:
1. Rewrite abstract to match final framing and claims.
2. Rewrite conclusion to emphasize practical diagnostic contribution and explicit limits.
3. Ensure consistency between title/subtitle and claim scope.

Acceptance checks:
1. Abstract and conclusion contain no claims broader than the body supports.
2. Final messaging aligns with short manuscript resolutions.

## Phase 7: Consistency and Risk Audit (Day 4)

Tasks:
1. Run risky-term search sweep across manuscript and related docs.
2. Run cross-file consistency pass between short and long manuscripts.
3. Produce a final unresolved-issues list requiring author judgement.

Acceptance checks:
1. Zero unresolved high-risk phrasing hits.
2. All remaining open issues are explicit and decision-oriented.

## Suggested Work Products Per Day

Day 1 deliverables:
1. Framing-lock commit or patch summary.
2. Risky-term hit reduction report.

Day 2 deliverables:
1. Methods + legitimacy protocol update summary.
2. Updated figures/tables interpretation notes.

Day 3 deliverables:
1. Discussion, abstract, conclusion update summary.
2. Remaining conflict list.

Day 4 deliverables:
1. Final consistency audit log.
2. Submission-ready claim map.

## Quick Command Pattern for Audit Passes

Search for framing and overclaim risk:
- correct model|true model|perfect structural knowledge|no prior reason
- not defined|new quantity|replaces EVPI|replaces BMA
- legitimacy|normalization choice|standing|exclusion cost

## Linked Inputs

Use these every pass:
1. ../potential_co_author_feedback/Mark_Strong/Mark_feedback_v1
2. ../potential_co_author_feedback/Keith_Shepherd/EVCA_IS_NOT_NEW.txt
3. ../potential_co_author_feedback/Keith_Shepherd/INFORMS_Review.txt
4. ../potential_co_author_feedback/Keith_Shepherd/ChatGPT_Review_KS.txt
5. ../potential_co_author_feedback/Keith_Shepherd/Claude_Review_KS.txt
6. ../manuscript_short/orl_short_communication.Rmd
7. /memories/repo/evca_review_findings.md
