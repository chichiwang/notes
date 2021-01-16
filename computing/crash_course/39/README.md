# Education Technology
[Video Link](https://youtu.be/zTi3_l5h5PQ)

One of the most dramatic changes enabled by computing technology has been the creation and widespread availability of information. There are currently 1.3 billion websites on the [internet](../glossary/README.md#internet). [Wikipedia](https://en.wikipedia.org/) alone has five million English language articles, spanning everything from the [Dancing Plague of 1518](https://en.wikipedia.org/wiki/Dancing_plague_of_1518) to proper [toilet paper roll orientation](https://en.wikipedia.org/wiki/Toilet_paper_orientation). Every day [Google](http://google.com/) serves up [four billion searches](https://www.internetlivestats.com/google-search-statistics/) to access this information, and every minute [3.5 million videos are viewed](https://fortunelords.com/youtube-stats/) on [Youtube](https://www.youtube.com/), and 400 hours of new video get uploaded by users. Many of these views are entertainment-related, however another large percentage could be considered educational. All of this information is a few clicks away for most people worldwide. But having information available is not the same as learning from it. Interactive in-class learning, directed conversations, and hands-on experiences are powerful tools for learning. [Educational technology](../glossary/README.md#educational-technology) provides an additive power to learning both inside and outside of tthe classroom.

Technology, from paper and pencil to recent [machine-learning](../glossary/README.md#machine-learning)-based intelligent systems, has been supporting education for millennia - even as early as humans drawing cave paintings to record hunting scenes for posterity. Teaching people at a distance has long been a driver of educational technology, for example: around 50 CE, [Saint Paul](https://en.wikipedia.org/wiki/Paul_the_Apostle) was sending epistles that offered lessons on religious teachings for new churches being set up in Asia. Several major waves of technological advances have each promised to revolutionize education, from radio and television, to DVDs and laserdiscs.

As far back as 1913 [Thomas Edison](https://en.wikipedia.org/wiki/Thomas_Edison) predicted:
> Books will soon be obsolete in the schools. It is possible to teach every branch of human knowledge with the motion picture. Our school system will be completely changed in the next ten years.

While that did not happen, distributing educational materials in formats like video has become more and more popular. Video is naturally adjustable - the viewer can ensure the pacing is right for them by using the video speed controls. Video can also be paused at difficult parts to muse on the content that has been presented. A viewer can also try examples or exercises that are presented in the video on their own to help cement the learning. [Active learning](../glossary/README.md#active-learning) techniques like these have been shown to increase learning by a factor of ten.

The idea of video as a way to spread quality education has held appeal for many over the last century. The latest incarnation of this idea are [massive open online courses](../glossary/README.md#massive-open-online-course) (_MOOCS_). The [New York Times](https://en.wikipedia.org/wiki/The_New_York_Times) actually declared 2012 the [Year of the MOOC](https://www.nytimes.com/2012/11/04/education/edlife/massive-open-online-courses-are-multiplying-at-a-rapid-pace.html). Early forms of MOOCs were just videos of lectures from famous professors. For a while some thought this might mean the end of universities as they knew them. That particular hyped future has not come to pass. When trying to scale up learning using technology to include millions of students simultaneously with small numbers of instructional staff (or even none at all) a lot of problems arise. These problems have intrigued computer scientists, and more specifically educational technologists, who are looking for solutions to them.

Effective learning involves receiving timely and relevant feedback. How can timely feedback be provided to millions of students under the tutelage of a single instructor? Solving many of these problems means creating hybrid human-technology systems. One useful, if contreversial approach, is that students can be a great resource in providing each other feedback. On the other hand students are not the best equipped to provide this feedback - they are neither subject-matter experts nor teachers. Their efforts, however, can be supported with technology. [Algorithms](../glossary/README.md#algorithm) can be used to match together perfect learning pairs, out of potentially millions of groupings. Automated systems can also do some of the grading while humans do the rest. For instance, computer algorithms that grade the writing portions of the [SATs](https://en.wikipedia.org/wiki/SAT) have been found to be just as accurate as humans hired to grade them by hand.

Other algorithms have been developed to provide personalized learning experiences (similar to [Netflix's](https://en.wikipedia.org/wiki/Netflix) personalized movie recommendations or [Google's](https://en.wikipedia.org/wiki/Google) personalized search results). To achieve this the software needs to understand what a learning does and does not know. The software can then present the right material, at the right time, to provide each learner practice on the topics and exercises most challenging to them. These systems, often powered by [artificial intelligence](../glossary/README.md#artificial-intelligence) are broadly called [intelligent tutoring systems](../glossary/README.md#intelligent-tutoring-system).

One such hypothetical system, following common conventions, may present a student with an algebra problem in the tutoring software:

```
3x + 7 = 4
```

The correct next step to solve the equation is to subtract both sides by 7:

```
3x + 7 = 4
   - 7   -7
3x = -3
```

The knowledge required to execute this step can be represented by something called a [production rule](../glossary/README.md#production). These describe procedures as IF-THEN statements. The [pseudocode](../glossary/README.md#pseudocode) of a production rule for this step would say:

```
IF there is a constant on the same side as a variable
THEN subtract that constant from both sides
```

Production rules can also be used to represent common mistake a student may make. These production rules are called _buggy rules_. For example, instead of subtracting the constant, a student may mistakenly try to subtract the coefficient:

```
IF there is a constant on the side side as a variable
THEN subtract the coefficient of the variable from both sides
```

It is possible that multiple, competing production rules are triggered after a student completes a step - it may not be entirely clear what misconception has led to a particular answer so production rules are combined with an algorithm that selects the most likely one. In this manner, the student can be given a helpful piece of feedback.

These production rules, and the selection algorithm, combine to form a [domain model](../glossary/README.md#domain-model): a formal representation of the knowledge, procedures, and skills of a particular discipline (such as algebra). Domain models can be used to assist learners on any individual problem but they prove insufficient in helping learners move through an entire cirriculum because they do not track progress over time. To accomplish this, intelligent tutoring systems build and maintain a _student model_ that tracks, among other things, what production rules a student has mastered and where they still need practice.

It is actually a sizeable challenge to determine what a student knows and doesn't know based only on their answers to problems. A common technique to figuring this out is [Bayesian Knowledge Tracing](../glossary/README.md#bayesian-knowledge-tracing). The algorithm treats student knowledge as a set of _latent variables_, which are variables whose true value is hidden from an outside observer like our software. This models the physical world, where a teacher would not know for certain whether a student knows a topic completely. Bayesian Knowledge Tracing updates its estimate of a students' knowledge by observing the correctness of each interation using that skill. To achieve this, the software maintains four probabilities:
1. The probability that a student has learned how to do a particular skill.
2. The probability of guess: the probability that a student arrived at a correct answer on accident.
3. The probability of slip: the probability that a student arrived at an incorrect answer due to careless error or mistake.
4. The probability of transit: the probability that a student started off a problem not knowing how to solve it, but learned the correct approach by working through the problem.

These four probabilities are used in a set of equations that update the student model, keeping a running assessment for each skill the student needs to master. The first equation asks: what's the probability that the student has learned a particular skill, which takes into account the probability that it was already learned previously, and the probability of transit.

P(Learned<sub>Now</sub>) = P(Learned<sub>Previous</sub>) + P(Transit) x (1 - P(Learned<sub>Previous</sub>))

The estimate of the probability that a skill was learned previously depends on whether a student gets a particular question correct or incorrect.

**Answer Was Correct**

P(Learned<sub>Previous</sub>) = (P(Learned<sub>Previous</sub>) x (1 - P(Slip))) / (P(Learned<sub>Previous</sub>) x (1 - P(Slip) + (1 - P(Learned<sub>Previous</sub>)) x P(Guess))

**Answer Was Incorrect**

P(Learned<sub>Previous</sub>) = (P(Learned<sub>Previous</sub>) x P(Slip)) / (P(Learned<sub>Previous</sub>) x P(Slip) + (1 - P(Learned<sub>Previous</sub>)) x (1 - P(Guess)))

After the correct value is computed, it is plugged into the first equation, updating the probability has learned a particular skill, which then gets stored in their student model.

Although there are many approaches, intelligent tutoring systems often use Bayesian Knowledge Tracing to support what's called [mastery learning](../glossary/README.md#mastery-learning), where students practice skills until they are deeply understood. To accomplish this goal the most efficiently, the software selects the best problems to present to the student to achieve mastery, a strategy called _adapative sequencing_, which is one form of personalization. Internet-connected educational applications or sites now allow teachers and researchers the ability to collect data from millions of learners. From that data, insights can be gleamed (like common pitfalls and topics/courses that cause frustration). This sort of insight can be gleaned from behavioral analytics such as how long students pause before entering an answer, where they speed up videos, and how they interact with other students on discussion forums. These strategies are part of a field called [Educational Data Mining](../glossary/README.md#educational-data-mining) which leverage their insights to help improve personalized learning in the future.

Educational technologies have often drawn their inspiration for their innovations from science fiction. Many researchers were inspired by the future envisioned in the book [The Diamond Age](https://en.wikipedia.org/wiki/The_Diamond_Age) by Neal Stephenson. This story describes a young girl who learns from a book that has a set of virtual agents who interact with her in natural language, acting as coaches, teachers, and mentors who grow and change with her as she grows up. They can detect what she knows and how she's feeling, and give just the right feedback and support to help her learn.

Today there are non-science-fiction researchers such as [Justine Cassell](https://en.wikipedia.org/wiki/Justine_Cassell) crafting [pedagogical virtual agents](../glossary/README.md#pedagogical-agent) that can "exhibit the verbal and bodily behaviors found in conversation among humans, and in doing so, build trust, rapport and even friendship with their human students."

Educational technology and devices are moving off of laptop and desktop computers onto huge tabletop surfaces, where students can collaborate in groups, and also onto tiny mobile devices where students can learn on the go. Virtual reality and augmented reality are also enabling new educational experience for learnings: diving deep under the oceans, exploring outer space, traveling through the human body, or interacting with cultures a student may never encounter in their lives. Projecting into the future: educational devices may one day be replaced by direct brain learning, where new skills can be uploaded directly into a student's brain. While this idea may seem far-fetched, scientists are already making inroads such as detecting if a person has knowledge of a particular topic just from reading their brain signals.
 
| [Previous: Psychology of Computing](../38/README.md) | [Table of Contents](../README.md#table-of-contents) | Next |
