#' @importFrom shiny HTML

instruction_list <- list(
     list("str" = "This is the main page. To start, click the user icon on the top right.",
          "img" = 'app_1main.png'),
     list("str" = HTML("Click the green <b>Start Labelling</b> button to load the first file."),
          "img" = 'app_2start.png'),
     list('str' = "This is the default display for the audio files. Use the embedded audio player to listen to the clip.",
          'img' = 'app_3display.png'),
     list('str' = "To filter the audio, drag a tight box around the audio of interest. The audio player will now only play between the times and frequencies in that selected area.",
          'img' = 'app_4select.png'),
     list('str' = "To save your selection as an annotation/label:",
          list('str' = "Click the species identified.",
               'img' = 'app_5aselectspecies.png'),
          list('str' = HTML("Click <code>Save Selection</code> to label the selection as the chosen species."),
               'img' = 'app_5bselection_saved.png')
          ),
     list('str' = "To add an extra species to the list of classes:",
          list('str' = HTML("Type the species into the <b>Type in additional category</b> box"),
               'img' = 'app_6typespecies.png'),
          list('str' = HTML("Press enter or click the <b>Add</b> button below"),
               'img' = 'app_6baddspecies.png')
          ),
     list('str' = "To investigate a selection further:",
          list('str' = "Double click the selected area to zoom.",
               'img' = 'app_7azoom.png'),
          list('str' = "The zoomed area can look a little blurry, we can change this with settings in the sidebar",
               'img' = 'app_7bsidebar.png'),
          list('str' = HTML("Open the <b>FFT Settings</b> drowpdown menu"),
               'img' = 'app_7cfftsettings.png'),
          list('str' = HTML("Increase the <b>FFT Overlap for display spectrogram (%)</b> parameter. The spectrogram will then reload with a better resolution"),
               'img' = 'app_7dfftadjust.png')
          )
)