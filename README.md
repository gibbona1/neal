# audio_labeler

TODOs:
- [ ] Instruction pdf:
  - [ ] Open in adjacent tab
  - [ ] Make sure instructions say how to zoom in/out
- [ ] Get ~1 hour of files in 4/5 folders
- [ ] Default logged in users to their folder, if guest or cant find folder, use tmp folder
- [x] Download labels button:
  - [x] Only I can download all labels (admin)
  - [x] Other users can download their own
- [ ] Display metadata:
  - [x] Collapsible panel under play audio
  - [x] Basic collapsible panel with formatted time display
  - [ ] Get location from recorder text files
  - [ ] Infer weather using some API (temperature, weather, wind, percipitation)
  - [ ] Get habitat type (either hard coded or gotten from wind farm GIS files)
- [ ] Make File Select dropdown easier to navigate
- [ ] File label stats (# labels)
- [ ] If keeping next/previous section buttons:
  - [ ] Little overlap between sections so boxes will not be cut short or duplicated
- [ ] Otherwise:
  - [ ] Scroll bar instead
  - [ ] If you haven't scrolled through all (or most of) the way right when clicking `next file`, give warning notification
- [x] Label name should always be visible if box is on screen (not just top left)
- [ ] `<NULL>` option shouldn't be visible in call type, file select etc
- [x] Change colour of save selection button
- [x] Any box with a label not in list doesn't show
- [x] Extra button border colour light blue
- [x] N+E Logo on login page (in branding tab of Auth0)
- [x] Empty space "header" above save selection
- [x] Reset button (sidebar, body, etc) to put all inputs back to default
- [ ] Interactive bounding boxes:
  - [ ] resizable
  - [ ] moveable
  - [ ] copy/paste-able
  - [ ] editing label and other info
  - [ ] 
- [x] Display logged in username in header
- [x] More icons for buttons:
  - [x] Body
  - [x] Sidebar
- [ ] navbarPage() to have distinct pages: label, verify/check, run model
  from https://shiny.rstudio.com/articles/layout-guide.html
- [ ] Save list of added species as col in species csv (or append to a column) 
- [ ] Ability to click Label boxes and see info/play sound
- [ ] Show details of saved labels in list in a sidebar
- [ ] On clicking label in sidebar, highlights the label (option to play it)
- [ ] Unit tests (especially for plots)
- [ ] Check soundgen pitch app https://github.com/tatters/soundgen
- [ ] Example sound files in right sidebar 
  (https://birdwatchireland.ie/our-work/surveys-research/research-surveys/
#countryside-bird-survey/cbs-bird-songs-and-calls/)
- [ ] Mel scale
- [ ] gridExtra blank plot with correct axes, paste spec as image (not raster)