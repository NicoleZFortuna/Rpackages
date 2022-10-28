# Data for the tellMyFortune function
magicAnswers <- c("It is certain",
                  "It is decidedly so",
                  "Without a doubt",
                  "Yes difinitely",
                  "You may rely on it",
                  "As I see it, yes",
                  "Most likely",
                  "Outlook good",
                  "Yes",
                  "Signs point to yes",
                  "Reply hazy, try again",
                  "Ask again later",
                  "Better not tell you now",
                  "Cannot predict now",
                  "Concentrate and ask again",
                  "Don't count on it",
                  "My reply is no",
                  "My sources say no",
                  "Outlook not so good",
                  "Very doubtful")

cookieAnswers <- c("The fortune you seek is in another cookie.",
            "A closed mouth gathers no feet.",
            "A conclusion is simply the place where you got tired of thinking.",
            "A cynic is only a frustrated optimist.",
            "A foolish man listens to his heart. A wise man listens to cookies.",
            "You will die alone and poorly dressed.",
            "A fanatic is one who can't change his mind, and won't change the subject.",
            "If you look back, you’ll soon be going that way.",
            "He who throws dirt is losing ground.",
            "The greatest danger could be your stupidity.",
            "The road to riches is paved with homework.",
            "May you someday be carbon neutral.",
            "Some fortune cookies contain no fortune.",
            "Only listen to the fortune cookie; disregard all other fortune telling units.")

tarotAnswers <- list("The Fool" = list(Up = "You are on the verge of an exciting, new adventure. A leap of faith may be required of you...",
                                Down = "Be alert to your surroundings. Consider the repercusions for your actions..."),
              "The Magician" = list(Up = "Take advantage of any upcoming opportunities. Big changes are to come...",
                                    Down = "Beware of false promises. If it looks to good to be true, it probably is..."),
              "The High Priestess" = list(Up = "You have started on a path to self discovery. Do not be distracted by the pull of capitalist success...",
                                          Down = "Ignor your heart at your own peril. The mind can play tricks..."),
              "The Empress" = list(Up = "Reconnect with the parts of your life that bring beauty and sensuality, such that you might become pregnant with joy...",
                                   Down = "You have lost sight of your inner goddess. F*** those b******. Find your peace..."),
              "The Emperor" = list(Up = "Through discipline and dedication, you will achieve your aims. Allow your ambition to take the reigns...",
                                   Down = "You have either become a tyrant, or have been running away from your responsibilities. Frankie says relax..."),
              "The Pope" = list(Up = "Do not stray from the path. Find comfort in the familiar...",
                                Down = "Break free from the shackles of convention. Down with the man..."),
              "The Lovers" = list(Up = "Where you once found conflict, you will find strength. The union of the seemingly disperate will bring enlightenment..." ,
                                  Down = "Your foundations have been shaken. Open up the channels of communication, with others and yourself..."),
              "The Chariot" = list(Up = "You must maintain focus against all obticals. Victory is on the horizon...",
                                   Down = "You have become at the mercy of outside forces. Gather your courage if you would not fall to chaos..."),
              "Justice" = list(Up = "The truth will come out. Justice will prevail...",
                               Down = "You are running from the truth. Your future depends on your acceptance of the past..."),
              "The Hermit" = list(Up = "You must go far from the maddening crowds. Others will distract you from finding your authentic self...",
                                  Down = "You have removed yourself too much from society. Maybe take a break from Netflix..."),
              "The Wheel of Fortune" = list(Up = "Nothing lasts forever, good or bad. The stream of life flows ever onward...",
                                            Down = "Lately nothing has gone right for you. Just hunker down. This too shall pass..."),
              "Strength" = list(Up = "Your inner strength and compassion for others is admirable. Keep it up.",
                                Down = "You will soon go through a crisis of faith. The road ahead is rocky..."),
              "The Hanged Man" = list(Up = "You will need to make a sacrifice. The thing you have been clinging to is holding you back...",
                                      Down = "You feel you have been going nowhere, despite making great efforts. The tides will soon turn..."),
              "Death" = list(Up = "The next phase of your life is about to begin. Let go of old baggage weighting you down...",
                             Down = "You have been clinging to the past. Beware that in doing so, you do not destroy your future..."),
              "Temperance" = list(Up = "You must avoid extremes. You must maintain balance in all things...",
                                  Down = "Your feelings of anxiety are a result of the lack of balance in your life. Reconsider your priorities..."),
              "The Devil" = list(Up = "You have become a slave to the material world. Beware, the things you own end up owning you...",
                                 Down = "You are in the process of kicking a nasty habit. Good for you!"),
              "The Tower" = list(Up = "You must abandon what you know to be false. It is time to forge a new path...",
                                 Down = "Your house is built on sand. Dude, it's gonna fall. Just let if fall."),
              "The Star" = list(Up = "You have within you everything you need to succeed. Don't give up hope...",
                                Down = "You feel like the world has turned against you. Remember, without hope you will never succeed..."),
              "The Moon" = list(Up = "You must let go of deep seated fears. Let your intuition guide you forward...",
                                Down = "Your anxieties have been clouding your judgement. Do not be decieved by your inner demons..."),
              "The Sun" = list(Up = "Your radience brings inspiration to all. Keep doing what you're doing.",
                               Down = "You feel like things keep getting in your way. Take steps to rebuild your confidence..."),
              "Judgement" = list(Up = "You are entering a period of self reflection. Be honest with yourself.",
                                 Down = "You are too harsh on yourself. Of course you do not get opportunities when you talk about yourself the way you do."),
              "The World" = list(Up = "Like Siddartha before you, you are walking the path to enlightenment. Yay you.",
                                 Down = "You are coming to the end of a long journey, and yet you feel empty. What a shame."))

# If you run this function with no input, all fortunes will be given. Otherwise you can specify
# "Magic8Ball", "FortuneCookie", or "TarotCards"
tellMyFortune <- function(fortuneTeller = NA) {
  if (is.na(fortuneTeller)) {
    theWorks = TRUE
    fortuneTeller = FALSE
  } else if (!fortuneTeller %in% c("Magic8Ball", "FortuneCookie", "TarotCards")) {
    writeLines("You have not provided a legitimate fortune teller. We will consult all available entities...\n")
  }

  if (fortuneTeller == "Magic8Ball") {
    return(paste0("The magic 8 ball says... ", sample(magicAnswers, 1)))
  }

  if (fortuneTeller == "FortuneCookie") {
    return(paste0("The mystical cookie says... ", sample(cookieAnswers, 1)))
  }

  if (fortuneTeller == "TarotCards") {
    card = sample(length(tarotAnswers), 1)
    orientation = sample(2, 1)
    return(paste0("The tarot card that you have drawn is...  ", names(tarotAnswers)[[card]],
                      " in the ", c("upright", "reverse")[orientation], " orientation.",
                       tarotAnswers[[card]][[orientation]]))
  }

  if (theWorks == TRUE) {
    writeLines("Ok, let's do this...")
    Sys.sleep(2)

    writeLines("In response to that burning question you have been ruminating over...")
    Sys.sleep(3)
    writeLines(paste0("The magic 8 ball says... ", sample(magicAnswers, 1)))
    Sys.sleep(2)

    writeLines("Alright, now for the fortune cookie...")
    Sys.sleep(3)
    writeLines(paste0("The mystical cookie says... ", sample(cookieAnswers, 1)))
    Sys.sleep(2)

    writeLines("And finally, let us read your tarot...")
    Sys.sleep(3)
    card = sample(length(tarotAnswers), 1)
    orientation = sample(2, 1)
    writeLines(paste0("The tarot card that you have drawn is...  ", names(tarotAnswers)[[card]],
                      " in the ", c("upright", "reverse")[orientation], " orientation. ",
                      tarotAnswers[[card]][[orientation]]))
  }
}

# n is how many numbers into the fibonacci sequence that you want to calculate. Final
# is a logical value for if you want to only return the final value in the sequence
fibonacci <- function(n, final = FALSE) {
  if (n < 0) {
    n = abs(n)
    warning("You have provided a negative number. Computing the Fibonacci sequence for ", n)
  }

  if (n == 0) {
    stop("You need to provide a whole number larger than 0")
  }

  if (n == 1 | n == 2) {
    return(1)
  }

  n = rep(1, n)
  for (i in 3:length(n)) {
    n[i] = sum(n[(i-2):(i-1)])
  }

  if (final == FALSE) {
    return(n)
  } else {
    return(tail(n, 1))
  }
}

# Must provide vectors of length 4 for each argument
MASH <- function(kids, jobs, spouses) {
  if (length(kids) !=4 | length(jobs) !=4 | length(spouses) !=4) {
    stop("Each argument must be a vector of length 4.")
  }

  housing <- c("Mansion", "Apartment", "Shack", "House")
  count <- sample(3:9, 1)

  fut <- data.frame(class = c(rep("house", 4), rep("kids", 4),
                             rep("jobs", 4), rep("spouses", 4)),
                   val = c(housing, kids, jobs, spouses))

  final <- data.frame(class = c("house", "kids", "jobs", "spouses"),
                     val = NA)

  j <- 0
  while (sum(is.na(final$val))> 0) { # while we do not have all final vals
    for (i in 1:nrow(fut)) {
      if (!is.na(fut$val[i])) {
        j <- j + 1
        if (j == count) {
          if (length(which(fut$class[!is.na(fut$val)] == fut$class[i])) == 1) {
            # if there is only one within the category
            final$val[final$class == fut$class[i]] <- fut$val[i]
            fut$val[i] <- NA
          } else {
            fut$val[i] <- NA
          }
          j <- 0
        }
      }
    }
  }

  class(final) = "MASH"
  return(final)
}

# provide output of the MASH function
print.MASH <- function(x, ...) {
  if (x$val[x$class == "kids"] %in% c(1, "one", "One")) {
    return(sprintf("You will live in an %s. You will have %s kid. You will be a %s. You will be married to %s.",
                   x$val[1], x$val[2], x$val[3], x$val[4]))
  } else {
    return(sprintf("You will live in an %s. You will have %s kids. You will be a %s. You will be married to %s.",
                   x$val[1], x$val[2], x$val[3], x$val[4]))
  }
}

# n is how many numbers into the square series that you want to calculate. Final
# is a logical value for if you want to only return the final value in the sequence
squareSeries <- function(n, final = FALSE) {
  if (n < 0) {
    n = abs(n)
    warning("You have provided a negative number. Computing the square series sequence for ", n)
  }

  if (n == 0) {
    stop("You need to provide a whole number larger than 0")
  }

  if (n == 1) {
    return(1)
  }

  n = rep(1, n)
  j = 3
  for (i in 2:length(n)) {
    n[i] <- n[i-1] + j
    j = j + 2
  }

  n
}

# n is how many numbers into the triangle series that you want to calculate. Final
# is a logical value for if you want to only return the final value in the sequence
triangleSeries <- function(n, final = FALSE) {
  if (n < 0) {
    n = abs(n)
    warning("You have provided a negative number. Computing the triangle series sequence for ", n)
  }

  if (n == 0) {
    stop("You need to provide a whole number larger than 0")
  }

  if (n == 1) {
    return(1)
  }

  n = rep(1, n)
  j = 2
  for (i in 2:length(n)) {
    n[i] <- n[i-1] + j
    j = j + 1
  }

  n
}

# fill specifies the background colour. Provide the number of dots that you
# want to plot. ... for additional arguments to be passed to geom_point()
# code taken from: https://roweyerboat.github.io/drawing_flowers_with_r_and_ggplot2
library(ggplot2)
picture <- function(fill = "white", points = 500, ...) {
  angle <- pi*(3 - sqrt(5))

  t <- (1:points) * angle
  x <- sin(t)
  y <-cos(t)
  df <- data.frame(t, x, y)

  p <- ggplot(df, aes(x*t, y*t))
  p + geom_point(...) + theme(panel.background = element_rect(fill=fill),
                              panel.grid=element_blank(),
                              axis.ticks=element_blank(),
                              axis.title=element_blank(),
                              axis.text=element_blank())
}


