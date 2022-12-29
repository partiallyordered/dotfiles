This could also be thought of as "subtracting" the first branch from the second. Directly from the
`git rebase` documentation:

       Here is how you would transplant a topic branch based on one branch to another, to pretend that
       you forked the topic branch from the latter branch, using rebase --onto.

       First let’s assume your topic is based on branch next. For example, a feature developed in
       topic depends on some functionality which is found in next.

               o---o---o---o---o  master
                    \
                     o---o---o---o---o  next
                                      \
                                       o---o---o  topic

       We want to make topic forked from branch master; for example, because the functionality on
       which topic depends was merged into the more stable master branch. We want our tree to look
       like this:

               o---o---o---o---o  master
                   |            \
                   |             o'--o'--o'  topic
                    \
                     o---o---o---o---o  next

       We can get this using the following command:

           git rebase --onto master next topic
