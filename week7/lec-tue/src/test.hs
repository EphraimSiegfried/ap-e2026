load = do bible <- readFile "bible2.txt"
          let bibleWords = words bible
          let bibleWordOccs = zip bibleWords [1..]
          return bibleWordOccs
       