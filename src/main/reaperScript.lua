
function CreateFolder(index, name)
  reaper.InsertTrackAtIndex(index, false)
  folder = reaper.GetTrack(0, index)
  reaper.GetSetMediaTrackInfo_String(folder, 'P_NAME', name, true)
  reaper.SetMediaTrackInfo_Value( folder, 'I_FOLDERDEPTH',1)
  reaper.SetMediaTrackInfo_Value(folder, 'I_FOLDERCOMPACT', 0)
end

function ImportAudio(index, channel, trackName, fileName, lastInFolder)
  local folderDepth = 0
  if lastInFolder then folderDepth = -1 end

  reaper.SetEditCurPos(0, false, false)
  reaper.InsertTrackAtIndex(index, false)
  tr = reaper.GetTrack(0, index)
  reaper.GetSetMediaTrackInfo_String(tr, 'P_NAME', trackName, true)
  reaper.SetMediaTrackInfo_Value( tr, 'I_FOLDERDEPTH',folderDepth)
  reaper.SetOnlyTrackSelected(tr, true)
  reaper.InsertMedia(fileName, 0)
  item = reaper.GetTrackMediaItem(tr, 0)
  take = reaper.GetActiveTake(item)
  reaper.SetMediaItemTakeInfo_Value(take, "I_CHANMODE", channel + 64 + 2)
end

audioFile = "/Users/danielstahl/Documents/Music/Pieces/Ambient Music/Ambient Music 7/stage/ambientMusic7Score.caf"

CreateFolder(0, "Pulse parts")
ImportAudio(1, 1, "Pulse 1", audioFile, false)
ImportAudio(2, 3, "Pulse 1 effect", audioFile, false)
ImportAudio(3, 5, "Pulse 2", audioFile, false)
ImportAudio(4, 7, "Pulse 2 effect", audioFile, false)
ImportAudio(5, 9, "Pulse 3", audioFile, false)
ImportAudio(6, 11, "Pulse 3 effect", audioFile, true)

CreateFolder(7, "Osc part")
ImportAudio(8, 13, "Osc 1", audioFile, false)
ImportAudio(9, 15, "Osc 1 effect", audioFile, false)
ImportAudio(10, 17, "Osc 2", audioFile, false)
ImportAudio(11, 19, "Osc 2 effect", audioFile, false)
ImportAudio(12, 21, "Osc 3", audioFile, false)
ImportAudio(13, 23, "Osc 3 effect", audioFile, true)

CreateFolder(14, "Noise part")
ImportAudio(15, 25, "Noise 1", audioFile, false)
ImportAudio(16, 27, "Noise 1 effect", audioFile, false)
ImportAudio(17, 29, "Noise 2", audioFile, false)
ImportAudio(18, 31, "Noise 2 effect", audioFile, false)
ImportAudio(19, 33, "Noise 3", audioFile, false)
ImportAudio(20, 35, "Noise 3 effect", audioFile, true)
