function [pahandle] = audioInitialize(repetitions, wavfilename)
%HR edited 2/28/17
%TCS edited 5/16/17 trying to make sound not be truncated
% BasicSoundOutputDemo([repetitions=0][, wavfilename])
%
% Demonstrates very basic use of the Psychtoolbox sound output driver
% PsychPortAudio(). PsychPortAudio is a better, more reliable, more accurate
% replacement for the old Psychtoolbox SND() function and other means of
% sound output in Matlab like sound(), soundsc(), wavplay(), audioplayer()
% etc.
%
% This demo only demonstrates normal operation, not the low-latency mode,
% extra demos and tests for low-latency and high precision timing output will
% follow soon. If you need low-latency, make sure to read "help
% InitializePsychSound" carefully or contact the forum.
% Testing for low-latency mode showed that sub-millisecond accurate sound
% onset and < 10 msecs latency are possible on Linux, OSX and on some specially
% configured MS-Windows ASIO sound card setups.
%
%
% Optional arguments:
%
% repetitions = Number of repetitions of the sound. Zero = Repeat forever
% (until stopped by keypress), 1 = Play once, 2 = Play twice, ....
%
% wavfilename = Name of a .wav sound file to load and playback. Otherwise
% the good ol' handel.mat file (part of Matlab) is used.
%
% The demo just loads and plays the soundfile, waits for a keypress to stop
% it, then quits.

% History:
% 06/07/2007 Written (MK)

% Running on PTB-3? Abort otherwise.
AssertOpenGL;

if nargin < 1
    repetitions = [];
end

if isempty(repetitions)
    repetitions = 0;
end

% Filename provided?
if nargin < 2
    wavfilename = [];
end

if isempty(wavfilename)
    % No sound file provided. Load standard handel.mat of Matlab:
    load handel;
    nrchannels = 1; % One channel only -> Mono sound.
    freq = Fs;      % Fs is the correct playback frequency for handel.
    wavedata = y';  %#ok<NODEF> % Need sound vector as row vector, one row per channel.
else
    % Read WAV file from filesystem:
    [y, freq] = psychwavread(wavfilename);
    wavedata = y';
    nrchannels = size(wavedata,1); % Number of rows == number of channels.
end

% Make sure we have always 2 channels stereo output.
% Why? Because some low-end and embedded soundcards
% only support 2 channels, not 1 channel, and we want
% to be robust in our demos.
if nrchannels < 2
    wavedata = [wavedata ; wavedata];
    nrchannels = 2;
end

% Perform basic initialization of the sound driver:
InitializePsychSound;

% Open the default audio device [], with default mode [] (==Only playback),
% and a required latencyclass of zero 0 == no low-latency mode, as well as
% a frequency of freq and nrchannels sound channels.
% This returns a handle to the audio device:
try
    % Try with the 'freq'uency we wanted:
    pahandle = PsychPortAudio('Open', [], [], 0, freq, nrchannels);
catch
    % Failed. Retry with default frequency as suggested by device:
    fprintf('\nCould not open device at wanted playback frequency of %i Hz. Will retry with device default frequency.\n', freq);
    fprintf('Sound may sound a bit out of tune, ...\n\n');

    psychlasterror('reset');
    pahandle = PsychPortAudio('Open', [], [], 0, [], nrchannels);
end

% Fill the audio playback buffer with the audio data 'wavedata':
PsychPortAudio('FillBuffer', pahandle, wavedata);