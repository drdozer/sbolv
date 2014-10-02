package shared

import java.util.Date

import upickle._

object Helper {
  implicit class EnhancedReadWriter[A](val _rw: ReadWriter[A]) extends AnyVal {
    def compose[B](down: B => A, up: A => B): ReadWriter[B] = ReadWriter[B](
      _write = a => _rw.write(down(a)),
      _read = { case (b) => up(_rw.read(b)) })
  }
}

case class Story(storyId: Long,
                 chapterIds: List[Long],
                 creationDate: Date,
                 modificationDate: Date,
                 tags: List[String],
                 title: String)

object Story {
  import Helper._

//  implicit val storyPickler = Case6ReadWriter(Story.apply, Story.unapply)
  implicit val dateReadWrite: ReadWriter[Date] = (implicitly[ReadWriter[Long]]).compose(
      down = _.getTime,
      up = new Date(_) )


  val SlashDate = """(\d+)/(\d+)/(\d+)"""r
  val DashDate = """(\d+)-(\d+)-(\d+)"""r

  def asDate(dateString: String): Date = {
    dateString match {
      case SlashDate(month, day, year) =>
        new Date(year.toInt, month.toInt, day.toInt)
      case DashDate(year, month, day) =>
        new Date(year.toInt, month.toInt, day.toInt)
    }
  }

}

case class Stories(stories: List[Story])

case class WordCount(counts: Map[String, Int]) {
  def merge(wc: WordCount) = WordCount(
    counts ++ wc.counts.map { case (k, v) => k -> (v + counts.getOrElse(k, 0)) }
  )

  def byCounts = counts.to[Array].sortBy(0 - _._2)

  def asFrequencies: WordFrequency = {
    val total = counts.values.sum.toDouble
    WordFrequency(counts mapValues (c => c.toDouble / total))
  }
}

case class WordFrequency(frequencies: Map[String, Double]) {
  def byFrequencies = frequencies.to[Array].sortBy(0 - _._2)

  def merge(wf: WordFrequency) = WordFrequency(
    frequencies ++ wf.frequencies.map { case (k, v) => k -> (v + frequencies.getOrElse(k, 0.0)) }
  )

  def normalize = {
    val t = frequencies.values.sum
    WordFrequency(frequencies.mapValues(_ / t))
  }
}

case class MeanStdev(means: Map[String, Double], stdevs: Map[String, Double])

trait StoryDB[C[_]] {
  def all: C[Stories]
  def chapterText(chapterId: Long): C[String]
  def chapterWordCounts(chapterId: Long, preserveCase: Boolean): C[WordCount]
  def allWordCounts(preserveCase: Boolean): C[WordCount]
  def allMeanStdev: C[MeanStdev]
}

object StoryDB {
  def cache[C[_]](cached: StoryDB[C]): StoryDB[C] = new StoryDB[C] {
    lazy val all = cached.all

    private var chapterWordCounts_cache: Map[(Long, Boolean), C[WordCount]] = Map.empty
    override def chapterWordCounts(chapterId: Long, preserveCase: Boolean) = {
      val k = (chapterId, preserveCase)
      if(!chapterWordCounts_cache.contains(k)) {
        chapterWordCounts_cache = chapterWordCounts_cache + (k -> cached.chapterWordCounts(chapterId, preserveCase))
      }
      chapterWordCounts_cache(k)
    }

    var chapterText_cache: Map[Long, C[String]] = Map.empty
    override def chapterText(chapterId: Long) = {
      if(!chapterText_cache.contains(chapterId)) {
        chapterText_cache = chapterText_cache + (chapterId -> cached.chapterText(chapterId))
      }
      chapterText_cache(chapterId)
    }

    lazy val allWordCounts_cache_false = cached.allWordCounts(false)
    lazy val allWordCounts_cache_ture = cached.allWordCounts(true)

    override def allWordCounts(preserveCase: Boolean) =
      if(preserveCase) allWordCounts_cache_ture
      else allWordCounts_cache_false

    lazy val allMeanStdev = cached.allMeanStdev
  }
}