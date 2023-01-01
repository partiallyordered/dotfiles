
#### Interviewing
* Start with a quick couple of personal questions? Get the candidate relaxed?
* Start with easy questions, get the candidate relaxed, work up to harder questions, stop a line of
  inquiry when a candidate doesn't know an answer
* Ask about a problem we currently have and how the candidate would solve it
* When/if the candidate is stumped- prompt them with the correct answer, ask what they think of it
* Ask the candidate about both strengths and weaknesses of technologies they've used
* How does the candidate present their coding sample? Normally from the top down is the best,
    something like:
    1) starting at the manifest
    2) discussing dependencies
    3) discussing the repo file-system structure
    4) starting at the entrypoint
    5) discussing exception/error handling
    6) bottlenecks, limitations, shortcomings
* Does the candidate have production/scale experience?
    * Have they encountered scaling problems? How would they solve them?
    * Have they encountered nasty production issues/bugs? How did they solve them?
    * Where did they deploy to?
* Can we determine how well the candidate understands their tools?
* How does the candidate learn new skills?
* Does the candidate use shiny things when boring solutions would be sufficient?
* How does the candidate handle doing work they're not interested in?
* Does the candidate have confidence in what they _don't_ know? Are they comfortable admitting they
    don't know something? Make sure to ask some questions the candidate doesn't know the answer to.
* How can we ascertain:
    * reliability
    * honesty
    * responsibility
    * capability
    * empathy


#### Problem solving
Design a problem that has a range of solutions, each preferably utilising a different algorithm or
data structure.


#### Process, communication
* What environments has the candidate worked in?
    * What do they understand the term DevOps to mean?
        * Shift left
        * Single-piece work
    * What agile experience do they have? Can they describe Agile?
* What tools has the candidate used to manage process/communication?
* Has the candidate remote-worked previously?
    * How well is the candidate likely to communicate in a remote environment when having trouble
        understanding system/implementation/context?
* Version control
* Coding style
* PRs, collaboration on code
* How did the candidate manage dependency/version changes within/without their organisation?


#### Suggestions of questions the candidate might ask us?
* What is our development process?
* What is the structure of our team?
* What is the structure of our organisation as a whole?
* What would the candidate be working on?
* Is there an open-source component?


#### CV
* Are there skill or experience gaps?
* Does the CV display technical/communication/social/managerial/responsibility progression?


#### CS concepts
* Complexity?
    * Start with linked-list search. Better options in certain situations?


#### Common technologies
* Version control
* SQL
* Issue trackers
* Communication tools
* Language feature experience: procedural, functional, declarative, manual/automatic memory management


#### NodeJS
* Event loop. Pros/cons.
* Async. Single-threaded. Continuation-passing.
* libuv + v8, non-blocking IO
* What issues would the candidate have encountered if they'd been using a technology for a long
    time? Try to ask about those.


#### Coding tests
* Code structure
    * Is the code modular?
    * Is there much global state?
    * Requires vs. dependency injection?
    * Is the code testable?
* Documentation
    * How to work on the project?
    * How to build the project?
    * How to test the project?
    * How to execute the project?
    * How to deploy the project?
    * Is the API documented?
    * Formal API specification? (OpenAPI, JSONSchema, JSON Hyperschema?)
    * License?
    * Can I execute the project without risk somehow?
* Security
    * Is input validated?
    * Are all db queries parametrised or escaped?
* Repo structure
    * Is it obvious? Is the structure necessary?
* Technology choices
    * Are the technologies selected necessary? Sufficient? Good?
* Config
    * Is it taken from the environment?
    * Is it useful?
    * Is anything missing that shouldn't be?
    * Are there any secrets? Where are they?
* Testing
    * Dependency injection
    * Unit
    * Coverage
    * Integration
    * Load
    * Stress
    * Performance
    * Soak
    * Linting
    * Pre-commit/push hooks to run tests
    * Formal verification (?!)
* Language competency
    * Functional vs. procedural style
    * Declarative vs. procedural style
    * Execution model of the language
        * Sync/async
        * Event loop
        * Exceptions
        * OS threads/green threads/none
        * Memory management/garbage collection
        * Resource management in general
* Scale
    * Service interdependency
    * Connection pooling
    * Caching
    * Holding open connections to a backend/client instead of sending multiple
        requests/responses.
    * Subscribe to events from a data source rather than polling. More generally, evented vs.
        polling architectures.
    * Where might the bottlenecks be? Has the candidate demonstrated some awareness/handling of
        this?
* Robustness
    * How might the service respond to internal failure
* Logging
    * Is logging likely sufficient to diagnose problems outside of interactive usage, i.e. in a
        port-mortem or latent analysis?
    * Better: does the logging supply enough information about the inputs to the
        service/function/code that the error condition could be completely reproduced using the
        output from the logs, say in a unit test?
* Modularity
    * Could bits of the code be published separately, if necessary? Not that this is necessarily
        important, but it demonstrates a high degree of modularity.
* Dependencies
    * Are there trivial dependencies? left-pad? Dependencies, while crucial, present a security and
        maintenance risk.
    * Justification of dependencies
        * Has the candidate used their dependencies before?
        * Have they taken security considerations?
* Is there an API client library?
