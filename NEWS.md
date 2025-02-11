# requal 1.1.3 Rieppeleon

__Release notes:__  

- Navigation Enhancements
  - Document badges attached to segments are now clickable links, enabling seamless transitions from the Analyze screen to the Annotate screen.
- Quick Tagging
  - Users can now create new codes on the fly, with the option to edit them later.
- Code Editing Improvements
  - Codes can be edited directly within the Codebook screen, eliminating the need for the previous merge workaround.
- Highlight/Underline Toggle
  - Users can switch the display of annotated segments between highlight and underline styles.
- Adjustable Annotation Columns
  - Customize the layout by adjusting the ratio between text and coding tools to suit your workflow.
- Enhanced Two-Column View and Draggable Code Menu
  - Greater flexibility with additional options for displaying codes in the Annotation screen.
- Line counter
  - Track and reference specific locations in your document with the new line counter.
- Minor Fixes and Updates
- New Tests

# requal 1.1.2 Thuringothyris

__Release notes:__  

- Export functions
  - Export enabled for memos and codebooks. Segments export now includes character positions.
- Citation information
  - Updated citation information for proper referencing.
- User Interface
  - Minor UI updates plus a new logo.
- Improved testing
  - Certain behaviors are now formally tested with the integration of `shinytest2`.
- Permission controls in server mode
  - Project owners will no longer be able to inadvertently revoke their own permission privileges.

# requal 1.1.1

- export memos
- updated segment export (with segment start for easier narrative analysis)
- added CITATION file
- logo update
- citation information

# requal 1.1.0

- update UI
- add codebook export
- implement shinytest2 tests (see DEV-NOTES.md)

# requal 1.0.0

Version 1.0.0 Coelostegus is the final output of the Technology Agency of the Czech Republic project n. TL05000054.

__Features:__  

- modes  
  - "local" (run locally with SQLite)  
  - "server" (run remotely with PostgreSQL)  
- installation-wide user management with shinymanager  
- project-wide user management  
- manage user permissions  
- track user attributes. 
- add documents as txt files or copy&paste content  
- create codes and categories  
- annotate documents  
- analyze and export coded segments  
- report coding statistics and visualize agreement and consensus of coders  
- actions log  
- create free memos  
