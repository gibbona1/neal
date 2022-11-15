# Table of Contents
- [Instructions](https://github.com/gibbona1/audio_labeler/edit/master/instruction_doc/README.md#instructions)
- [Hotkeys](https://github.com/gibbona1/audio_labeler/edit/master/instruction_doc/README.md#hotkeys)

# Instructions

1. Login at https://rstudioserver.hamilton.ie:3939/content/35c6a672-2488-4079-bd75-f3d5d6d22789/

If there is a login, use your given Auth0 username and password. 

![Login](https://github.com/gibbona1/neal/blob/master/images/app_0login.png)

Sometimes there is a disconnect from the server. If this happens, reload or put the link https://rstudioserver.hamilton.ie:3939/content/35c6a672-2488-4079-bd75-f3d5d6d22789/ back into the address bar and press enter.

2. This is the main page. To start, click the user icon on the top right.

![Main Page](https://github.com/gibbona1/neal/blob/master/images/app_1main.png)

3. Click the green Start Labelling button to load the first file.

![Start Button](https://github.com/gibbona1/neal/blob/master/images/app_2start.png)

4. This is the default display for the audio files. Use the embedded audio player to listen to the clip.

![Display](https://github.com/gibbona1/neal/blob/master/images/app_3display.png)

5. To filter the audio, drag a tight box around the audio of interest. The audio player will now only play between the times and frequencies in that selected area.
![Display](https://github.com/gibbona1/neal/blob/master/images/app_4select.png)

6. To save your selection as an annotation/label:
    1. Click the species identified.
    ![Display](https://github.com/gibbona1/neal/blob/master/images/app_5aselectspecies.png)
    2. Click ``Save Selection`` to label the selection as the chosen species. 
    ![Display](https://github.com/gibbona1/neal/blob/master/images/app_5bselection_saved.png)

7. To add an extra species to the list of classes:
    1. Type the species into the *Type in additional category* box
    ![Display](https://github.com/gibbona1/neal/blob/master/images/app_6typespecies.png)
    2. Press enter or click the *Add* button below 
    ![Display](https://github.com/gibbona1/neal/blob/master/images/app_6baddspecies.png)

7. To investigate a selection further:
    1. Double click the selected area to zoom.
    ![Display](https://github.com/gibbona1/neal/blob/master/images/app_7azoom.png)
    2. The zoomed area can look a little blurry, we can change this with settings in the sidebar
    ![Display](https://github.com/gibbona1/neal/blob/master/images/app_7bsidebar.png)
    3. Open the *FFT Settings* drowpdown menu
    ![Display](https://github.com/gibbona1/neal/blob/master/images/app_7cfftsettings.png)
    4. Increase the *FFT Overlap for display spectrogram (%)* parameter. The spectrogram will then reload with a better resolution
    ![Display](https://github.com/gibbona1/neal/blob/master/images/app_7dfftadjust.png)

# Hotkeys
- <kbd>&#8679;</kbd>+<kbd>&#9166;</kbd> (<kbd>Shift</kbd> + <kbd>Enter</kbd>) to Save Selection
- <kbd>&#8679;</kbd>+<kbd>&#9003;</kbd> (<kbd>Shift</kbd> + <kbd>Backspace</kbd>) to Delete Selection
- <kbd>&#8679;</kbd>+<kbd>&#8592;</kbd> (<kbd>Shift</kbd>+<kbd>Left</kbd>) to move to previous file
- <kbd>&#8679;</kbd>+<kbd>&#8594;</kbd> (<kbd>Shift</kbd>+<kbd>Right</kbd>) to move to next file
- <kbd>&#8679;</kbd>+<kbd>Space</kbd> to pause/play audio
- <kbd>&#9166;</kbd> in additional category textbox to add one
- <kbd>&#9003;</kbd> in category list to delete one
