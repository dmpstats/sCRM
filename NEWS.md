# sCRM 0.2.0

## Bug Fixes

- Fixed issue #6, which revealed additional related bugs now also resolved. Notably:
  - Addressed disappearing UI panels caused by conflicts between `{rHandsontable}`, 
  Shiny modules, and server-side rendering. This required significant refactoring 
  of the source code.
  - Resolved `{shinyvalidate}` feedback not displaying when used in combination 
  with `waiter::Hostess()`. This was fixed by replacing `Hostess()` with `Garcon()`
  for webpage loading.

## Enhancements

- Minor UI improvements, including:
  - Font size adjustments for plot axis labels and legends.
  - Improved plot padding for better layout.
  - Updated logic for rendering species-level panels:
    - The "Demo Species" panel is now rendered only for the "Demo Windfarm" 
    workflow, preventing unintended displays in other workflows.

## Technical
- Package built under R 4.3.3 with all dependencies verified as up to date.
    



# sCRM 0.1.2
- Incorporated patch fixing bug in upload of FHD for non-default species in deterministic mode
- Added check on requirement for height column of uploaded FHDs starting from 0. Widget now issues error if data fails this requirement

# sCRM 0.1.1
- Added patch to amend issue in specification of blade pitch angle input on 
`band_crm()`, implying the conversion from degrees to radians (pull request #4).

# sCRM 0.1.0
- First version of the updated sCRM app, to be released for Beta testing
- Major changes from its predecessor tool [stochCRM](https://github.com/dmpstats/stochCRM) include:
  - Definition of multiple wind farm scenarios
  - Addition of seasonal outputs
  - Choose between Deterministic Vs. Stochastic modes
  - Added Option 4 estimates
  - Automated report generation
  - Integration with `{stochLAB}`, providing faster simulations.
  - A significant re-design of the UI structure to tackle the substantial
  increase in the number of required inputs, while keeping familiarity with the
  predecessor's workflow.
  - A significant rewrite (virtually from scratch) of the app's back-end logic,
  with focus on code modularity, streamlining and performance.

# sCRM 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.
