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
