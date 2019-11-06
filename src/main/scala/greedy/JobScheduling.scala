package greedy

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object JobScheduling extends App {
  /**
    * Job Scheduling Problem to minimize sum of completion times
    *   - INPUT DATA
    *     - We have a set of jobs, each having the following properties
    *       - Weight (Wj) or the importance of the job: jobs with higher weight should be
    *         processed earlier than the ones with a lower weight
    *       - Length (Lj): how much time the job needs to execute
    *     - Completion time for job j (Cj): the sum of the lengths of all the jobs scheduled
    *       before job j, including the length of job j
    *   - PROBLEM: find the job schedule that minimizes the sum of completion times for all jobs
    *     - MIN(SUM[j = 1 to n](wjCj))
    *   - HINT:
    *     - We should favor (schedule first) jobs that have a smaller weight
    *     - We should NOT favor (schedule as last as possible) jobs that have a larger length
    *     - The key is to find a "score function" that satisfies the 2 hints above
    *       - An optimal scoring function may be the ration  between the weight and the length (Wj/Lj)
    *       - An non-optimal scoring function is be the difference between the weight and the length (Wj-Lj)
    *   - SOLUTION:
    *     - For each job, we compute the ration  between the weight and the length (Wj/Lj)
    *     - We schedule the jobs in a decreasing order of these ratios
    *   - COMPLEXITY:
    *     - The complexity of the sorting algorithm: O(n*log(n))
    */

  case class Job(weight: Int, length: Int, ratio: Float)

  val filename = "src/main/resources/job_scheduling.txt"
  val iter = Source.fromFile(filename).getLines()
  val arr = new ArrayBuffer[Job]()
  for (line <- iter) {
    val items = line.split(" ")
    val w: Int = items(0).toInt
    val l: Int = items(1).toInt
    arr.addOne(Job(w, l, w.toFloat / l.toFloat))
  }

  implicit object JobRatioOrdering extends Ordering[Job] {
    override def compare(x: Job, y: Job): Int = x.ratio compare y.ratio
  }

  // Print the jobs in the order they should be scheduled.
  println("To minimize the sum of job completion times, jobs should be schedule in the following order:")
  println(arr.sorted.reverse)
}
