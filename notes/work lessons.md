Engage your stakeholders. The first step is to identify them. The second is to talk to them. The
third is to empathise with them. Are you a developer selecting a test framework? Your stakeholders
could be:
- SRE who would like to analyse/aggregate the results
- product owner, who would like to see test results in an easily consumable manner
- other developers, QA staff; who will write and run tests using the framework
- many more
This applies to programming languages, frameworks, libraries, communication tools, the code you
write (peer review is the process of getting approval from your stakeholders), the software you
produce (users!), the CI/CD platform and tools you use etc. All of these things affect
stakeholders- who are those stakeholders, and what do they need from your decision? How can your
decision make everyone's life _better_, rather than _worse_?

We work with people, not "resources". If you, in particular as management, refer to people as
"resources" or similar, you are treating them as a fungible commodity, and they are more likely to
respond in kind. Do not be surprised if their attitude toward you becomes transactional when
something goes awry; they'll have confirmed their preconception that you are a heartless machine
that can be treated correspondingly.

A dependency (library, service, whatever) is a liability. Of course, we use libraries to reduce the
amount of code we have to create and maintain. But dependencies have a maintenance and integration
cost. Do you have the resources available to keep the dependency up-to-date? Security audit each
version? Transition to a new major version? Transition to the dependency's successor when it's
deprecated or abandoned? What about transitive dependencies? Now you must wonder: do I really need
this dependency? Can I think about the problem differently? Can I solve the problem in a different
way?

Use the carrot, not the stick. No exceptions(?).

It's much nicer to work with your colleagues in terms of "we", not "you". Specific example:

> We missed that failure mode in our testing before we released the feature

instead of

> Nic missed that failure mode in their testing before they released the feature

Or, more cooperatively, when reviewing:

> Should we foo the bar, here? Perhaps we can skip that; it's only for display anyway


When something is well-designed and well-implemented, and doesn't break, nobody knows about it. In
terms of selfish, short-term career progression, it pays to have something fail occasionally and be
the hero/firefighter who saves the day. This is an unpleasant way to work. Just build good stuff in
the first place and leave if you're not appreciated. Sometimes it's possible to make up for this
effect with self-promotion.

Interpersonal communication during development is overhead that is fundamentally not generating
product. This is *not* to say that communication is a waste of time. A process/system that does not
minimise unnecessary communication is wasting resources. Some organisations seem to take the view
that visible communication is an unmitigated good, and therefore there should be more of it. This
is at odds with producing.
Obvious examples of good communication:
- the design phase
    - often input from others improves the result (quality/speed/simplicity)
    - need to actually be solving a business problem, not something else
- debugging: it's easy to focus on the wrong thing, sometimes a fresh set of eyes/questions is
    extremely useful
- relationships are built on communication, and it's nice to have friendly relationships with
    colleagues.

Developers must be able to run and interact with the software in a sufficiently representative
environment, with relative ease. If that's not possible, expect software quality and development
velocity to drop progressively.

Sometimes the business has to be told no, they can't have that quick hack. If they don't like that,
work for someone else.

There is no substitute for integration tests run before the code is released.

Reviews are about getting the first reader (non-writer) of your code to read and understand your
code. This is very important because, as the writer of your code, you're very poorly positioned to
analyse it as a reader. Remember the adage: "code is read 10x more than it's written", so it's very
important that it's accessible as a reader.

The biggest cost of code is maintenance, not implementation. Code that isn't written is code that
doesn't need to be maintained. Do you *really* need that in-house widget library?

Interfaces between teams should be defined in code as much as possible. This is one of the biggest
strengths of Kubernetes when used as an industry-standard ops/dev interface.

Assumptions we sometimes make when reviewing code:
- the code that hasn't been changed is okay
- the code has been tested, and works

Work with the primitives of your system. If the primitives of your system are users and medical
records and appointments, create interfaces (including development interfaces!) that work with
these primitives- *not* HTTP requests. IOW, Postman and curl are fine for exploratory usage, but
you'll work *much* more effectively if you can create a user with
```sh
$ my-cli create user --username jimmy --password correct-horse-battery-staple
```
or
```rust
const client = Client::new();
client.create_user("jimmy", "correct-horse-battery-staple");
```
instead of
```sh
$ kubectl port-forward svc/users 8080:80 &
$ curl --data '{ "blah": "blah" }' -H 'first: header' ... -H 'tenth: header' ... 'etc.'
```
or
```rust
const client = HttpClient::new();
const request = Request.builder()
    .header("first", "header")
    // ...
    .header("tenth", "header")
    .port(3030)
    .path("users")
    .method(http::Method::POST)
    .body(json::value!{
        "username": "jimmy",
        "password": "correct-horse-battery-staple",
    })
```