function (file) 
{
    BLOCKSIZE <- 512
    sectionNames <- c("ProtocolSection", "ADCSection", "DACSection", 
                      "EpochSection", "ADCPerDACSection", "EpochPerDACSection", 
                      "UserListSection", "StatsRegionSection", "MathSection", 
                      "StringsSection", "DataSection", "TagSection", "ScopeSection", 
                      "DeltaSection", "VoiceTagSection", "SynchArraySection", 
                      "AnnotationSection", "StatsSection")
    f <- file(file.choose(), open = "rb")
    on.exit(close(f))
    header <- list()
    skip <- function(n) invisible(readBin(f, "raw", n = n))
    bool <- function(n = 1) as.logical(readBin(f, n = n, "integer", 
                                               size = 1, endian = "little"))
    int8 <- function(n = 1) readBin(f, n = n, "integer", size = 1, 
                                    endian = "little")
    int16 <- function(n = 1) readBin(f, n = n, "integer", size = 2, 
                                     endian = "little")
    int32 <- function(n = 1) readBin(f, n = n, "integer", size = 4, 
                                     endian = "little")
    int64 <- function(n = 1) readBin(f, n = n, "integer", size = 8, 
                                     endian = "little")
    uint32 <- function(n = 1) {
        result <- c()
        for (i in 1:n) {
            lo <- readBin(f, "integer", size = 2, signed = FALSE, 
                          endian = "little")
            hi <- readBin(f, "integer", size = 2, signed = FALSE, 
                          endian = "little")
            result[i] <- 65536 * hi + lo
        }
        result
    }
    float32 <- function(n = 1) readBin(f, n = n, "double", size = 4, 
                                       endian = "little")
    char <- function(n = 1) readChar(f, rep(1, n), useBytes = TRUE)
    chararr <- function(n = 1) readChar(f, rep(n, 16), useBytes = TRUE)
    `%:%` <- function(a, b) header[[a]] <<- b()
    `%x%` <- function(f, n) function() f(n)
    header$fFileSignature <- readChar(f, nchars = 4, useBytes = TRUE)
    `ABF ` <- function() {
        "fFileVersionNumber" %:% float32
        "nOperationMode" %:% int16
        "lActualAcqLength" %:% int32
        "nNumPointsIgnored" %:% int16
        "lActualEpisodes" %:% int32
        skip(4)
        "lFileStartTime" %:% int32
        skip(12)
        "lDataSectionPtr" %:% int32
        "lTagSectionPtr" %:% int32
        "lNumTagEntries" %:% int32
        skip(40)
        "lSynchArrayPtr" %:% int32
        "lSynchArraySize" %:% int32
        "nDataFormat" %:% int16
        skip(18)
        "nADCNumChannels" %:% int16
        "fADCSampleInterval" %:% float32
        skip(4)
        "fSynchTimeUnit" %:% float32
        skip(4)
        "lNumSamplesPerEpisode" %:% int32
        "lPreTriggerSamples" %:% int32
        "lEpisodesPerRun" %:% int32
        skip(94)
        "fADCRange" %:% float32
        skip(4)
        "lADCResolution" %:% int32
        skip(110)
        "nFileStartMillisecs" %:% int16
        skip(10)
        "nADCPtoLChannelMap" %:% (int16 %x% 16)
        "nADCSamplingSeq" %:% (int16 %x% 16)
        "sADCChannelName" %:% (chararr %x% 10)
        "sADCUnits" %:% (chararr %x% 8)
        "fADCProgrammableGain" %:% (float32 %x% 16)
        skip(128)
        "fInstrumentScaleFactor" %:% (float32 %x% 16)
        "fInstrumentOffset" %:% (float32 %x% 16)
        "fSignalGain" %:% (float32 %x% 16)
        "fSignalOffset" %:% (float32 %x% 16)
        skip(3334)
        "nTelegraphEnable" %:% (int16 %x% 16)
        skip(32)
        "fTelegraphAdditGain" %:% (float32 %x% 16)
    }
    ABF2 <- function() {
        "uFileVersionNumber" %:% (int8 %x% 4)
        "uFileInfoSize" %:% uint32
        "lActualEpisodes" %:% uint32
        "uFileStartDate" %:% uint32
        "uFileStartTimeMS" %:% uint32
        "uStopwatchTime" %:% uint32
        "nFileType" %:% int16
        "nDataFormat" %:% int16
        "nSimultaneousScan" %:% int16
        "nCRCEnable" %:% int16
        "uFileCRC" %:% uint32
        "FileGUID" %:% (int32 %x% 4)
        "uCreatorVersion" %:% uint32
        "uCreatorNameIndex" %:% uint32
        "uModifierVersion" %:% uint32
        "uModifierNameIndex" %:% uint32
        "uProtocolPathIndex" %:% uint32
    }
    if (header$fFileSignature == "ABF ") {
        `ABF `()
        header$fFileVersionNumber <- round(header$fFileVersionNumber, 
                                           2)
        header$lFileStartTime <- header$lFileStartTime + header$nFileStartMillisecs * 
            0.001
    }
    else if (header$fFileSignature == "ABF2") {
        ABF2()
        header$fFileVersionNumber <- sum(header$uFileVersionNumber * 
                                             (1e-04) * 10^(1:4))
        header$lFileStartTime <- header$uFileStartTimeMS * 0.001
    }
    else {
        stop("unknown or incompatible file signature")
    }
    sections <- list()
    channelNames <- c()
    channelUnits <- c()
    ADCInfo <- function(offset) {
        seek(f, offset)
        list(nADCNum = int16(), nTelegraphEnable = int16(), nTelegraphInstrument = int16(), 
             fTelegraphAdditGain = float32(), fTelegraphFilter = float32(), 
             fTelegraphMembraneCap = float32(), nTelegraphMode = int16(), 
             fTelegraphAccessResistance = float32(), nADCPtoLChannelMap = int16(), 
             nADCSamplingSeq = int16(), fADCProgrammableGain = float32(), 
             fADCDisplayAmplification = float32(), fADCDisplayOffset = float32(), 
             fInstrumentScaleFactor = float32(), fInstrumentOffset = float32(), 
             fSignalGain = float32(), fSignalOffset = float32(), 
             fSignalLowpassFilter = float32(), fSignalHighpassFilter = float32(), 
             nLowpassFilterType = int8(), nHighpassFilterType = int8(), 
             fPostProcessLowpassFilter = float32(), nPostProcessLowpassFilterType = int8(), 
             bEnabledDuringPN = bool(), nStatsChannelPolarity = int16(), 
             lADCChannelNameIndex = int32(), lADCUnitsIndex = int32())
    }
    TagInfo <- function(offset) {
        seek(f, offset)
        list(lTagTime = int32(), sComment = (char %x% 56)(), 
             nTagType = int16(), nVoiceTagNumberOrAnnotationIndex = int16())
    }
    if (header$fFileVersionNumber >= 2) {
        seek(f, 76)
        for (name in sectionNames) {
            sections[[name]] <- list(uBlockIndex = uint32(), 
                                     uBytes = uint32(), llNumEntries = int64())
        }
        seek(f, sections$StringsSection$uBlockIndex * BLOCKSIZE)
        strings <- readBin(f, what = "character", sections$StringsSection$uBytes)
        keywords <- c("clampex", "clampfit", "axoscope", "patchxpress")
        matches <- sapply(keywords, function(s) {
            sapply(strings, function(r) {
                grepl(s, r, ignore.case = TRUE, useBytes = TRUE)
            })
        })
        for (i in seq_along(strings)) {
            if (rowSums(matches)[i] > 0) {
                strings <- strings[i:length(strings)]
                break
            }
        }
        ADCsec <- list()
        for (i in 1:sections$ADCSection$llNumEntries) {
            ADCsec[[i]] <- ADCInfo(sections$ADCSection$uBlockIndex * 
                                       BLOCKSIZE + sections$ADCSection$uBytes * (i - 
                                                                                     1))
            ii <- ADCsec[[i]]$nADCNum + 1
            header$nADCSamplingSeq[i] <- ADCsec[[i]]$nADCNum
            channelNames <- c(channelNames, strings[ADCsec[[i]]$lADCChannelNameIndex])
            unitsIndex <- ADCsec[[i]]$lADCUnitsIndex
            if (unitsIndex > 0) {
                channelUnits <- c(channelUnits, strings[unitsIndex])
            }
            header$nTelegraphEnable[ii] <- ADCsec[[i]]$nTelegraphEnable
            header$fTelegraphAdditGain[ii] <- ADCsec[[i]]$fTelegraphAdditGain
            header$fInstrumentScaleFactor[ii] <- ADCsec[[i]]$fInstrumentScaleFactor
            header$fSignalGain[ii] <- ADCsec[[i]]$fSignalGain
            header$fADCProgrammableGain[ii] <- ADCsec[[i]]$fADCProgrammableGain
            header$fInstrumentOffset[ii] <- ADCsec[[i]]$fInstrumentOffset
            header$fSignalOffset[ii] <- ADCsec[[i]]$fSignalOffset
        }
        seek(f, sections$ProtocolSection$uBlockIndex * BLOCKSIZE)
        ProtocolSec <- list(nOperationMode = int16(), fADCSequenceInterval = float32(), 
                            bEnableFileCompression = bool(), sUnused1 = (int8 %x% 
                                                                             3)(), uFileCompressionRatio = uint32(), fSynchTimeUnit = float32(), 
                            fSecondsPerRun = float32(), lNumSamplesPerEpisode = int32(), 
                            lPreTriggerSamples = int32(), lEpisodesPerRun = int32(), 
                            lRunsPerTrial = int32(), lNumberOfTrials = int32(), 
                            nAveragingMode = int16(), nUndoRunCount = int16(), 
                            nFirstEpisodeInRun = int16(), fTriggerThreshold = float32(), 
                            nTriggerSource = int16(), nTriggerAction = int16(), 
                            nTriggerPolarity = int16(), fScopeOutputInterval = float32(), 
                            fEpisodeStartToStart = float32(), fRunStartToStart = float32(), 
                            lAverageCount = int32(), fTrialStartToStart = float32(), 
                            nAutoTriggerStrategy = int16(), fFirstRunDelayS = float32(), 
                            nChannelStatsStrategy = int16(), lSamplesPerTrace = int32(), 
                            lStartDisplayNum = int32(), lFinishDisplayNum = int32(), 
                            nShowPNRawData = int16(), fStatisticsPeriod = float32(), 
                            lStatisticsMeasurements = int32(), nStatisticsSaveStrategy = int16(), 
                            fADCRange = float32(), fDACRange = float32(), lADCResolution = int32(), 
                            lDACResolution = int32(), nExperimentType = int16(), 
                            nManualInfoStrategy = int16(), nCommentsEnable = int16(), 
                            lFileCommentIndex = int32(), nAutoAnalyseEnable = int16(), 
                            nSignalType = int16(), nDigitalEnable = int16(), 
                            nActiveDACChannel = int16(), nDigitalHolding = int16(), 
                            nDigitalInterEpisode = int16(), nDigitalDACChannel = int16(), 
                            nDigitalTrainActiveLogic = int16(), nStatsEnable = int16(), 
                            nStatisticsClearStrategy = int16(), nLevelHysteresis = int16(), 
                            lTimeHysteresis = int32(), nAllowExternalTags = int16(), 
                            nAverageAlgorithm = int16(), fAverageWeighting = float32(), 
                            nUndoPromptStrategy = int16(), nTrialTriggerSource = int16(), 
                            nStatisticsDisplayStrategy = int16(), nExternalTagType = int16(), 
                            nScopeTriggerOut = int16(), nLTPType = int16(), nAlternateDACOutputState = int16(), 
                            nAlternateDigitalOutputState = int16(), fCellID = (float32 %x% 
                                                                                   3)(), nDigitizerADCs = int16(), nDigitizerDACs = int16(), 
                            nDigitizerTotalDigitalOuts = int16(), nDigitizerSynchDigitalOuts = int16(), 
                            nDigitizerType = int16())
        header$nOperationMode <- ProtocolSec$nOperationMode
        header$fSynchTimeUnit <- ProtocolSec$fSynchTimeUnit
        header$nADCNumChannels <- sections$ADCSection$llNumEntries
        header$lActualAcqLength <- sections$DataSection$llNumEntries
        header$lDataSectionPtr <- sections$DataSection$uBlockIndex
        header$nNumPointsIgnored <- 0
        header$fADCSampleInterval <- ProtocolSec$fADCSequenceInterval/header$nADCNumChannels
        header$fADCRange <- ProtocolSec$fADCRange
        header$lADCResolution <- ProtocolSec$lADCResolution
        header$lSynchArrayPtr <- sections$SynchArraySection$uBlockIndex
        header$lSynchArraySize <- sections$SynchArraySection$llNumEntries
    }
    else {
        sections$TagSection$llNumEntries <- header$lNumTagEntries
        sections$TagSection$uBlockIndex <- header$lTagSectionPtr
        sections$TagSection$uBytes <- 64
    }
    if (header$lActualAcqLength < header$nADCNumChannels) {
        stop("less data points than sampled channels in file")
    }
    recChIdx <- header$nADCSamplingSeq[1:header$nADCNumChannels]
    chInd <- seq(along = recChIdx)
    if (header$fFileVersionNumber < 2) {
        channelNames <- header$sADCChannelName[recChIdx + 1]
        channelUnits <- header$sADCUnits[recChIdx + 1]
    }
    if (header$fFileVersionNumber >= 1.65) {
        addGain <- header$nTelegraphEnable * header$fTelegraphAdditGain
        addGain[addGain == 0] <- 1
    }
    else {
        addGain <- rep(1, length(header$fTelegraphAdditGain))
    }
    if (header$nDataFormat == 0) {
        dataSz <- 2
        precision <- "int16"
    }
    else if (header$nDataFormat == 1) {
        dataSz <- 4
        precision <- "float32"
    }
    else {
        stop("invalid number format")
    }
    headOffset <- header$lDataSectionPtr * BLOCKSIZE + header$nNumPointsIgnored * 
        dataSz
    samplingIntervalInSec <- header$nADCNumChannels * header$fADCSampleInterval * 
        1e-06
    nSweeps <- header$lActualEpisodes
    sweeps <- 1:nSweeps
    if (header$fSynchTimeUnit == 0) {
        header$synchArrTimeBase <- 1
    }
    else {
        header$synchArrTimeBase <- header$fSynchTimeUnit
    }
    tags <- list()
    TagSec <- list()
    for (i in seq(length.out = sections$TagSection$llNumEntries)) {
        tmp <- TagInfo(sections$TagSection$uBlockIndex * BLOCKSIZE + 
                           sections$TagSection$uBytes * (i - 1))
        timeSinceRecStartInSec <- if (header$fFileVersionNumber >= 
                                      2) {
            tmp$lTagTime * ProtocolSec$fADCSequenceInterval/(1e+06 * 
                                                                 sections$ADCSection$llNumEntries)
        }
        else {
            tmp$lTagTime * header$synchArrTimeBase/1e+06
        }
        tags[[i]] <- list(timeSinceRecStartInSec = timeSinceRecStartInSec, 
                          comment = trimws(paste(tmp$sComment, collapse = "")))
        TagSec[[i]] <- tmp
    }
    if (header$nOperationMode == 1) {
        if (header$fFileVersionNumber >= 2) {
            stop("this reader currently does not work with data acquired in ", 
                 "event-driven variable-length mode and ABF version 2.0")
        }
        else {
            if (header$lSynchArrayPtr <= 0 || header$lSynchArraySize <= 
                0) {
                stop("internal variables 'lSynchArray*' are zero or negative")
            }
            header$lSynchArrayPtrByte <- BLOCKSIZE * header$lSynchArrayPtr
            seek(f, header$lSynchArrayPtrByte)
            synchArr <- int32(header$lSynchArraySize * 2)
            if (length(synchArr) != header$lSynchArraySize * 
                2) {
                stop("something went wrong while reading synch array section")
            }
            synchArr <- t(matrix(synchArr, nrow = 2))
            segLengthInPts <- synchArr[, 2]/header$synchArrTimeBase
            seek(f, headOffset)
            d <- list()
            for (i in seq(length.out = nSweeps)) {
                readerFunction <- list(int16 = int16, float32 = float32)[[precision]]
                tmpd <- readerFunction(segLengthInPts[sweeps[i]])
                n <- length(tmpd)
                if (n != segLengthInPts[sweeps[i]]) {
                    warning("something went wrong while reading episode ", 
                            sweeps[i], ": ", segLengthInPts[sweeps[i]], 
                            " points should have been read, ", n, " points actually read")
                }
                header$dataPtsPerChan <- n/header$nADCNumChannels
                tmpd <- matrix(tmpd, header$dataPtsPerChan, header$nADCNumChannels, 
                               byrow = TRUE)
                if (!header$nDataFormat) {
                    for (j in 1:length(chInd)) {
                        ch <- recChIdx[chInd[j]] + 1
                        tmpd[, j] <- tmpd[, j]/(header$fInstrumentScaleFactor[ch] * 
                                                    header$fSignalGain[ch] * header$fADCProgrammableGain[ch] * 
                                                    addGain[ch]) * header$fADCRange/header$lADCResolution + 
                            header$fInstrumentOffset[ch] - header$fSignalOffset[ch]
                    }
                }
                d[[i]] <- tmpd
            }
        }
    }
    else if (header$nOperationMode %in% c(2, 4, 5)) {
        if (header$lSynchArrayPtr <= 0 || header$lSynchArraySize <= 
            0) {
            stop("internal variables 'lSynchArray*' are zero or negative")
        }
        header$lSynchArrayPtrByte <- BLOCKSIZE * header$lSynchArrayPtr
        seek(f, header$lSynchArrayPtrByte)
        synchArr <- int32(header$lSynchArraySize * 2)
        if (length(synchArr) != header$lSynchArraySize * 2) {
            stop("something went wrong while reading synch array section")
        }
        synchArr <- t(matrix(synchArr, nrow = 2))
        if (length(unique(synchArr[, 2])) > 1) {
            stop("sweeps of unequal length in file recorded in fixed-length mode")
        }
        header$sweepLengthInPts <- synchArr[1, 2]/header$nADCNumChannels
        header$sweepStartInPts <- synchArr[, 1] * (header$synchArrTimeBase/header$fADCSampleInterval/header$nADCNumChannels)
        header$recTime <- header$lFileStartTime
        tmpvar <- header$sweepStartInPts[length(header$sweepStartInPts)]
        header$recTime <- header$recTime + c(0, (1e-06 * (tmpvar + 
                                                              header$sweepLengthInPts)) * header$fADCSampleInterval * 
                                                 header$nADCNumChannels)
        header$dataPts <- header$lActualAcqLength
        header$dataPtsPerChan <- header$dataPts/header$nADCNumChannels
        dataPtsPerSweep <- header$sweepLengthInPts * header$nADCNumChannels
        seek(f, headOffset)
        d <- list()
        selectedSegStartInPts <- (sweeps - 1) * dataPtsPerSweep * 
            dataSz + headOffset
        for (i in seq(length.out = nSweeps)) {
            seek(f, selectedSegStartInPts[i])
            readerFunction <- list(int16 = int16, float32 = float32)[[precision]]
            tmpd <- readerFunction(dataPtsPerSweep)
            n <- length(tmpd)
            if (n != dataPtsPerSweep) {
                stop("something went wrong while reading episode ", 
                     sweeps[i], ": ", dataPtsPerSweep, " points should have been read, ", 
                     n, " points actually read")
            }
            header$dataPtsPerChan <- n/header$nADCNumChannels
            tmpd <- matrix(tmpd, header$dataPtsPerChan, header$nADCNumChannels, 
                           byrow = TRUE)
            if (!header$nDataFormat) {
                for (j in 1:length(chInd)) {
                    ch <- recChIdx[chInd[j]] + 1
                    tmpd[, j] <- tmpd[, j]/(header$fInstrumentScaleFactor[ch] * 
                                                header$fSignalGain[ch] * header$fADCProgrammableGain[ch] * 
                                                addGain[ch]) * header$fADCRange/header$lADCResolution + 
                        header$fInstrumentOffset[ch] - header$fSignalOffset[ch]
                }
            }
            d[[i]] <- tmpd
        }
    }
    else if (header$nOperationMode == 3) {
        header$dataPtsPerChan <- header$lActualAcqLength/header$nADCNumChannels
        header$dataPts <- header$dataPtsPerChan * header$nADCNumChannels
        totalLength <- 1e-06 * header$lActualAcqLength * header$fADCSampleInterval
        header$recTime <- header$lFileStartTime
        header$recTime <- c(header$recTime, header$recTime + 
                                totalLength)
        seek(f, headOffset)
        tmpd <- list(int16 = int16, float32 = float32)[[precision]](header$dataPts)
        n <- length(tmpd)
        if (n != header$dataPts) {
            stop("something went wrong while reading file (", 
                 header$dataPts, " points should have been read, ", 
                 n, " points actually read")
        }
        tmpd <- matrix(tmpd, header$dataPtsPerChan, header$nADCNumChannels, 
                       byrow = TRUE)
        if (!header$nDataFormat) {
            for (j in 1:length(chInd)) {
                ch <- recChIdx[chInd[j]] + 1
                tmpd[, j] <- tmpd[, j]/(header$fInstrumentScaleFactor[ch] * 
                                            header$fSignalGain[ch] * header$fADCProgrammableGain[ch] * 
                                            addGain[ch]) * header$fADCRange/header$lADCResolution + 
                    header$fInstrumentOffset[ch] - header$fSignalOffset[ch]
            }
        }
        d <- list()
        d[[1]] <- tmpd
    }
    else {
        stop("unknown recording mode")
    }
    if (length(tags) > 0 && !is.null(header$sweepStartInPts)) {
        for (i in 1:length(tags)) {
            tmp <- which(tags[[i]]$timeSinceRecStartInSec >= 
                             header$sweepStartInPts * samplingIntervalInSec)
            tags[[i]]$episodeIndex <- tmp[length(tmp)]
        }
    }
    Encoding(channelNames) <- "latin1"
    Encoding(channelUnits) <- "latin1"
    result <- list(path = normalizePath(file), formatVersion = sprintf("%.2f", 
                                                                       header$fFileVersionNumber), channelNames = trimws(channelNames), 
                   channelUnits = trimws(channelUnits), samplingIntervalInSec = samplingIntervalInSec, 
                   data = d, tags = tags, header = header, sections = list(rawSections = sections, 
                                                                           ADCsec = if (exists("ADCsec")) ADCsec, ProtocolSec = if (exists("ProtocolSec")) ProtocolSec, 
                                                                           TagSec = TagSec))
    class(result) <- "ABF"
    result
}
<bytecode: 0x000000000291f0d8>
    <environment: namespace:readABF>