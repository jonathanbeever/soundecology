#Multiple sounds
#
# Script to process all .wav files in a directory and save
# the requested index to a .csv file.
#
#

multiple_sounds <- function(directory, soundindex = c("ndsi", "acoustic_complexity", "acoustic_diversity", "acoustic_evenness", "bioacoustic_index", "H"), no_cores = 1, flac = FALSE, from = NA, to = NA, units = NA, ...) {

  if (any(soundindex %in% c("ndsi", "acoustic_complexity", "acoustic_diversity", "acoustic_evenness", "bioacoustic_index", "H")) == FALSE) {
    stop(paste("Unknown function", soundindex))
  }

  if (file.access(directory) == -1) {
    stop(paste("The directory specified does not exist or this user is not autorized to read it:\n    ", directory))
  }

  if (is.na(from) == FALSE) {
    if (is.na(to) || is.na(units)) {
      stop("All three arguments 'from', 'to', and 'units' must be specified.")
    }
  }
  if (is.na(to) == FALSE) {
    if (is.na(from) || is.na(units)) {
      stop("All three arguments 'from', 'to', and 'units' must be specified.")
    }
  }
  if (is.na(units) == FALSE) {
    if (is.na(from) || is.na(to)) {
      stop("All three arguments 'from', 'to', and 'units' must be specified.")
    }
  }

  #How many cores this machine has?
  #require(parallel)
  thismachine_cores <- detectCores()

  if (no_cores == 0) {
    stop("Number of cores can not be 0.")
  }else if (no_cores < -1) {
    stop("Number of cores can not be negative.")
  }else if (no_cores == "max") {
    no_cores <- thismachine_cores
  }else if (no_cores == -1) {
    no_cores <- thismachine_cores - 1
  }else if (no_cores > thismachine_cores) {
    #Don't try to use more than the number of cores in the machine
    warning(paste(" The number of cores to use can not be more than the
					  cores in this computer: ", detectCores()), immediate. = TRUE)
    no_cores <- thismachine_cores
  }

  #Are the files flac
  if (flac == TRUE) {
    wav_files <- dir(path = directory, pattern = "flac$", ignore.case = TRUE)
    if (length(wav_files) == 0) {
      stop(paste("Could not find any .flac files in the specified directory:\n    ", directory))
    }
  }else {
    wav_files <- dir(path = directory, pattern = "wav$", ignore.case = TRUE)
    if (length(wav_files) == 0) {
      stop(paste("Could not find any .wav files in the specified directory:\n    ", directory))
    }
  }

  #Open flac files
  get_wav <- function(directory, flacsoundfile) {
    #is it windows?
    if (.Platform$OS.type == "windows") {
      from_file <- paste(directory, "\\", flacsoundfile, sep = "")
    }else {
      from_file <- paste(directory, "/", flacsoundfile, sep = "")
    }

    wav_file <- paste(strtrim(flacsoundfile, nchar(flacsoundfile) - 5), "wav", sep = ".")

    file.copy(from = from_file, to = flacsoundfile)
    wav2flac(flacsoundfile, reverse = TRUE, overwrite = TRUE)
    file.remove(flacsoundfile)
    if (file.exists(wav_file)) {
      return(wav_file)
    }else {
      return(NA)
    }
  }

  #Bioacoustic index
  if (soundindex == "bioacoustic_index") {

    getindex <- function(soundfile, inCluster = FALSE, ...) {
      #If launched in cluster, require the package for each node created
      if (inCluster == TRUE) {
        require(soundecology)
      }

      #Get args
      args <- list(...)

      if (!is.null(args[['min_freq']])) {
        min_freq <- args[['min_freq']]
      }else {
        min_freq <- formals(bioacoustic_index)$min_freq
      }
      if (!is.null(args[['max_freq']])) {
        max_freq <- args[['max_freq']]
      }else {
        max_freq <- formals(bioacoustic_index)$max_freq
      }
      if (!is.null(args[['fft_w']])) {
        fft_w <- args[['fft_w']]
      }else {
        fft_w <- formals(bioacoustic_index)$fft_w
      }

      if (flac == TRUE) {
        soundfile_path <- get_wav(directory, soundfile)
      }else {
        if (.Platform$OS.type == "windows") {
          soundfile_path <- paste(directory, "\\", soundfile, sep = "")
        }else {
          soundfile_path <- paste(directory, "/", soundfile, sep = "")
        }
      }

      if (is.na(from) == FALSE) {
        this_soundfile <- readWave(soundfile_path, from = from, to = to, units = units)
      }else {
        this_soundfile <- readWave(soundfile_path)
      }

      return_list <- bioacoustic_index(this_soundfile, ...)

      if (this_soundfile@stereo == TRUE) {
        no_channels <- 2
      }else {
        no_channels <- 1
      }

      if (flac == TRUE) {
        file.remove(soundfile_path)
      }

      return(return_list)
    }

  }else if (soundindex == "acoustic_diversity") {
    #Acoustic Diversity

    getindex <- function(soundfile, inCluster = FALSE, ...) {
      #If launched in cluster, require the package for each node created
      if (inCluster == TRUE) {
        require(soundecology)
      }

      #Get args
      args <- list(...)

      if (!is.null(args[['db_threshold']])) {
        db_threshold <- args[['db_threshold']]
      }else {
        db_threshold <- formals(acoustic_diversity)$db_threshold
      }
      if (!is.null(args[['max_freq']])) {
        max_freq <- args[['max_freq']]
      }else {
        max_freq <- formals(acoustic_diversity)$max_freq
      }
      if (!is.null(args[['freq_step']])) {
        freq_step <- args[['freq_step']]
      }else {
        freq_step <- formals(acoustic_diversity)$freq_step
      }

      if (flac == TRUE) {
        soundfile_path <- get_wav(directory, soundfile)
      }else {
        if (.Platform$OS.type == "windows") {
          soundfile_path <- paste(directory, "\\", soundfile, sep = "")
        }else {
          soundfile_path <- paste(directory, "/", soundfile, sep = "")
        }
      }

      if (is.na(from) == FALSE) {
        this_soundfile <- readWave(soundfile_path, from = from, to = to, units = units)
      }else {
        this_soundfile <- readWave(soundfile_path)
      }

      return_list <- acoustic_diversity(this_soundfile, ...)

      if (this_soundfile@stereo == TRUE) {
        no_channels <- 2
      }else {
        no_channels <- 1
      }

      if (flac == TRUE) {
        file.remove(soundfile_path)
      }

      return(return_list)
    }

  }else if (soundindex == "acoustic_complexity") {
    #Acoustic Complexity

    getindex <- function(soundfile, inCluster = FALSE, ...) {
      #If launched in cluster, require the package for each node created
      if (inCluster == TRUE) {
        require(soundecology)
      }

      #Get args
      args <- list(...)

      if (!is.null(args[['max_freq']])) {
        max_freq <- args[['max_freq']]
      }else {
        max_freq <- formals(acoustic_complexity)$max_freq
      }
      if (!is.null(args[['min_freq']])) {
        min_freq <- args[['min_freq']]
      }else {
        min_freq <- 1
      }
      if (!is.null(args[['j']])) {
        j <- args[['j']]
      }else {
        j <- formals(acoustic_complexity)$j
      }
      if (!is.null(args[['fft_w']])) {
        fft_w <- args[['fft_w']]
      }else {
        fft_w <- formals(acoustic_complexity)$fft_w
      }

      if (flac == TRUE) {
        soundfile_path <- get_wav(directory, soundfile)
      }else {
        if (.Platform$OS.type == "windows") {
          soundfile_path <- paste(directory, "\\", soundfile, sep = "")
        }else {
          soundfile_path <- paste(directory, "/", soundfile, sep = "")
        }
      }

      if (is.na(from) == FALSE) {
        this_soundfile <- readWave(soundfile_path, from = from, to = to, units = units)
      }else {
        this_soundfile <- readWave(soundfile_path)
      }

      return_list <- acoustic_complexity(this_soundfile, ...)

      if (this_soundfile@stereo == TRUE) {
        no_channels <- 2
      }else {
        no_channels <- 1
      }

      if (flac == TRUE) {
        file.remove(soundfile_path)
      }

      return(return_list)
    }

  }else if (soundindex == "ndsi") {
    #NDSI

    getindex <- function(soundfile, inCluster = FALSE, ...) {
      #If launched in cluster, require the package for each node created
      if (inCluster == TRUE) {
        require(soundecology)
      }

      #Get args
      args <- list(...)

      if (!is.null(args[['fft_w']])) {
        fft_w <- args[['fft_w']]
      }else {
        fft_w <- formals(ndsi)$fft_w
      }
      if (!is.null(args[['anthro_min']])) {
        anthro_min <- args[['anthro_min']]
      }else {
        anthro_min <- formals(ndsi)$anthro_min
      }
      if (!is.null(args[['anthro_max']])) {
        anthro_max <- args[['anthro_max']]
      }else {
        anthro_max <- formals(ndsi)$anthro_max
      }
      if (!is.null(args[['bio_min']])) {
        bio_min <- args[['bio_min']]
      }else {
        bio_min <- formals(ndsi)$bio_min
      }
      if (!is.null(args[['bio_max']])) {
        bio_max <- args[['bio_max']]
      }else {
        bio_max <- formals(ndsi)$bio_max
      }

      if (flac == TRUE) {
        soundfile_path <- get_wav(directory, soundfile)
      }else {
        if (.Platform$OS.type == "windows") {
          soundfile_path <- paste(directory, "\\", soundfile, sep = "")
        }else {
          soundfile_path <- paste(directory, "/", soundfile, sep = "")
        }
      }

      if (is.na(from) == FALSE) {
        this_soundfile <- readWave(soundfile_path, from = from, to = to, units = units)
      }else {
        this_soundfile <- readWave(soundfile_path)
      }

      return_list <- ndsi(this_soundfile, ...)

      if (this_soundfile@stereo == TRUE) {
        no_channels <- 2
      }else {
        no_channels <- 1
      }

      if (flac == TRUE) {
        file.remove(soundfile_path)
      }

      return(return_list)
    }

  }else if (soundindex == "H") {
    #H

    getindex <- function(soundfile, inCluster = FALSE, ...) {
      #If launched in cluster, require the package for each node created
      if (inCluster == TRUE) {
        require(soundecology)
      }

      #Get args
      args <- list(...)

      if (!is.null(args[['wl']])) {
        wl <- args[['wl']]
      }else {
        wl <- formals(H)$wl
      }
      if (!is.null(args[['envt']])) {
        envt <- args[['envt']]
      }else {
        envt <- formals(H)$envt
      }
      if (!is.null(args[['msmooth']])) {
        msmooth <- args[['msmooth']]
      }else {
        msmooth <- formals(H)$msmooth

        if (is.null(msmooth)) {
          msmooth <- "NULL"
        }
      }
      if (!is.null(args[['ksmooth']])) {
        ksmooth <- args[['ksmooth']]
      }else {
        ksmooth <- formals(H)$ksmooth

        if (is.null(ksmooth)) {
          ksmooth <- "NULL"
        }
      }

      if (flac == TRUE) {
        soundfile_path <- get_wav(directory, soundfile)
      }else {
        if (.Platform$OS.type == "windows") {
          soundfile_path <- paste(directory, "\\", soundfile, sep = "")
        }else {
          soundfile_path <- paste(directory, "/", soundfile, sep = "")
        }
      }


      if (is.na(from) == FALSE) {
        this_soundfile <- readWave(soundfile_path, from = from, to = to, units = units)
      }else {
        this_soundfile <- readWave(soundfile_path)
      }

      if (this_soundfile@stereo == TRUE) {
        left <- channel(this_soundfile, which = c("left"))
        right <- channel(this_soundfile, which = c("right"))
        left_res <- H(left, ...)
        right_res <- H(right, ...)
      }else {
        left <- channel(this_soundfile, which = c("left"))
        left_res <- H(left, ...)
        right_res <- NA
      }

      if (this_soundfile@stereo == TRUE) {
        no_channels <- 2
      }else {
        no_channels <- 1
      }

      if (flac == TRUE) {
        file.remove(soundfile_path)
      }

      return(return_list)
    }

  }else if (soundindex == "acoustic_evenness") {
    #Acoustic evenness

    getindex <- function(soundfile, inCluster = FALSE, ...) {
      #If launched in cluster, require the package for each node created
      if (inCluster == TRUE) {
        require(soundecology)
      }

      #Get args
      args <- list(...)

      if (!is.null(args[['db_threshold']])) {
        db_threshold <- args[['db_threshold']]
      }else {
        db_threshold <- formals(acoustic_evenness)$db_threshold
      }
      if (!is.null(args[['max_freq']])) {
        max_freq <- args[['max_freq']]
      }else {
        max_freq <- formals(acoustic_evenness)$max_freq
      }
      if (!is.null(args[['freq_step']])) {
        freq_step <- args[['freq_step']]
      }else {
        freq_step <- formals(acoustic_evenness)$freq_step
      }

      if (flac == TRUE) {
        soundfile_path <- get_wav(directory, soundfile)
      }else {
        if (.Platform$OS.type == "windows") {
          soundfile_path <- paste(directory, "\\", soundfile, sep = "")
        }else {
          soundfile_path <- paste(directory, "/", soundfile, sep = "")
        }
      }


      if (is.na(from) == FALSE) {
        this_soundfile <- readWave(soundfile_path, from = from, to = to, units = units)
      }else {
        this_soundfile <- readWave(soundfile_path)
      }

      return_list <- acoustic_evenness(this_soundfile, ...)

      if (this_soundfile@stereo == TRUE) {
        no_channels <- 2
      }else {
        no_channels <- 1
      }

      if (flac == TRUE) {
        file.remove(soundfile_path)
      }

      return(return_list)
    }

  }

  results <- list()

  #Start timer
  time0 <- proc.time()

  #Use parallel?
  if (no_cores > 1) {
    #require(parallel)
    no_files <- length(wav_files)

    if (no_cores > no_files) {
      no_cores <- no_files
      cat("\n The number of cores to use has been reduced because there are less files than cores available\n")
    }

    cat(paste("\n Running the function ", soundindex, "() on ", no_files, " files using ", no_cores, " cores", "\n\n", sep = ""))

    cl <- makeCluster(no_cores, type = "PSOCK")

    results <- parLapply(cl, wav_files, getindex, inCluster = TRUE, ...)

    #pause to allow all to end
    Sys.sleep(1)

    stopCluster(cl)
  }else {

    cat(paste(" Running on ", length(wav_files), " files using 1 core", "\n\n", sep = ""))

    for (soundfile in wav_files) {
      this_res <- getindex(soundfile, ...)
      results <- append(results, this_res)
    }
  }

  #Stop timer
  time1 <- proc.time() - time0
  cat(paste(" The analysis of ", length(wav_files), " files took ", round(time1["elapsed"], 2), " seconds\n\n", sep = ""))

  return(results)
}
