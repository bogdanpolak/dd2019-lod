# Good programming practices

### Teamwork

* Leave a trace after your work
     * A shared repository
     * With all-day coding - min. 10 revisions (commits)
* Plan and write your tasks
* Developers only internal system (GitHub, GitLab, etc.)
* Source code reviews (peer review)
* Check code in pairs on more difficult topics
     * Pair Programming - it's one of the requirements of the XP methodology

### Code Smells

* Creativity and learning
    * Do not program continuously in the same way. Try to learn and experiment with new coding techniques.
* Style Guide
    * What is good style and what is bad style
    * Code formatting
    * Code easy and difficult to read
    * The rules of pure code
    * Build the proper principles, adjust to your team and projects
    * Learn to recognize bad code smells

### DRY

DRY - Don't Repeat Yourself

* Avoid copy-paste (copy-pasta coding)
* Generalize the solution
    * Use records, objects, arrays and lists
    * Find repeated patterns and build reusable modules
    * Use very few IF / WHILE conditional statements in the methods
* When copy-paste ?
    * First time - OK with TODO
    * Second time - OK with TODO
    * Third time - no way - generalize this code and reuse

### Clean code in Delphi

* Zero warnings
    - Remove all warnings and hints from you projects
* Uses clean-ups
    - Scheduled and repeated uses cleaning procedure
* Use class helpers
    - Simplify code which are playing with the VCL and RTL classes / components
    - Extract it into the reusable class helpers
* Rename identifiers
    - Change names (classes, methods, parameters and variables) to more descriptive ones
* Size
    - Small and easy to read methods
* Object Oriented Programming
    - Write new classes: encapsulate, generalize, 
    - Try not to write independent functions and procedures

### SOLID Principles

1. SRP = Single Responsibility Principle
    * Class should be repressible for the one thing only
    * Example: processing orders and storing orders are two responsibilities
    * Look on the classic TForm class
2. OCP = Open/Close Principle
    * It's about OOP inheritance
    * Open for extension, but closed for modification
    * Suspicious code: multi-level ifs / case
    * Use the Command and Strategy GoF patterns
3. LSP = Liskov Substitution Principle
    * Objects in a program should be replaceable with instances of their subtypes without altering the correctness of that program
    * It's about proper inheritance and use of the abstract classes
    * Be aware when overwriting inherited methods, do not replace parent code just extend it
    * wrong:
        ```
        TSqare = class(TRectangle)
        ```
    * correct: 
        ```
        TFigure = abstract class
        TSqare = class(TFigure)
        TRectangle = class(TFigure)
        ```
4. ISP = Interface Segregation Principle
    * It's about using interfaces
    * Many client-specific interfaces are better than one general-purpose interface.
    * Divide the interfaces into smaller ones (domain-specific)
5. DIP = Dependency Invention Principle
    * It's about of decoupling software modules using interfaces
    * Classes from two logically different layers should be separated by interfaces
    * Introduce to IoC Principle - Inversion of Control
    * Three Musketeers: DIP, IoC, DI (Dependency Injection)
    
### Law of Demeter - LoD

* Principle of Least Knowledge
* Talk only with your friends, never with strangers
* It's about loose coupling (decoupling)
* Objects are less dependent on the other objects and easier to test and to maintain

[slideshare.net presentation](https://www.slideshare.net/vladimirtsukur/law-of-demeter-objective-sense-of-style)

### Testing

* Test often, test quickly and test early
* Automation
* Unit tests
    * Safety net
    * Allow developers to make more dangerous refactoring
    * Technical debt - simple tasks are become very difficult to implement (like monetary deb - borrowing cash allows you to speed up the business, but is also dangerous)
    * Requires small and independent classes
    * Delphi: 
        * DUnitX
        * TestInsight (Stefan Glienke) [link](https://bitbucket.org/sglienke/testinsight/wiki/Home)
* Acceptance tests
    * Record, generalize and play
    * Requires dedicated tools (expensive ones)
        * Ranorex Studio - [link](https://www.ranorex.com/windows-desktop-test-automation/)
        * Smartbear TestComplete - [link](https://smartbear.com/product/testcomplete/)
