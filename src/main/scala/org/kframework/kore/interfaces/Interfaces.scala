package org.kframework.kore.interfaces

/**
  * Provides a collection of [[tree.AST]] types, allowing for viewing [[pattern.Pattern]]s as ASTs.
  *
  * Sample Usage -
  * {{{def printVariableNames(pattern: Pattern): Unit = p match {
  *     case BinderNode(Variable(str: Name, Sort(sortName)), p: Pattern) => {
  *       println(Name +"@" + sortName)
  *       printVariableName(p)
  *     }
  *     case Node(args: Seq[Pattern]) => args.map(printVariableNames)
  *     case Variable(str: Name, Sort(sortName)) => println(str + "@" sortName)
  *   }
  * }}}
  *
  *
  */
object tree {

  //import pattern._

  /**
    * Base type of the Tree interface. [[pattern.Pattern]] extends AST.
    * Represents a Pattern in Matching Logic.
    */
  sealed trait AST[T <: AST[T]]


  /**
    * Specifies Node behavior of [[pattern.Pattern]].
    *
    * Allows matching a Pattern against as a Node,
    * and building a Pattern from a list of Patterns.
    */
  sealed trait Node[T <: AST[T]] extends AST[T] with Product {
    def args: Seq[T]

    /* Allows building Nodes using a list of Patterns */
    def build(children: Seq[T]): T
  }

  object Node {
    def unapply(arg: Node[_]): Option[Seq[_]] = Some(arg.args)
  }


  /**
    * Specifies Leaf Behavior of Patterns.
    *
    * @tparam C The Contents of the Leaf.
    */
  sealed trait Leaf[C, T <: AST[T]] extends AST[T] with Product {
    def contents: C

    /* Allows building a leaf using its contents */
    def build(contents: C): T
  }

  object Leaf {
    def unapply[C](arg: Leaf[C, _]): Option[C] = Some(arg.contents)
  }


  /**
    * A Leaf with Product2[CC1, CC2] as its contents. [[pattern.DomainValue]], [[pattern.Variable]] extend this trait.
    *
    * [[Leaf2]] extends [[Leaf]], with Product[CC1, CC2] as its type parameter.
    * This allows for creating a Leaf[Product[CC1, CC2]], directly using the build method, of
    * type (CC1, CC2) => Pattern.
    *
    * @tparam CC1 Type of First Field.
    * @tparam CC2 Type of Second Field.
    */
  sealed trait Leaf2[CC1, CC2, T <: AST[T]] extends Leaf[Product2[CC1, CC2], T] with Product2[CC1, CC2] {
    override def contents: Product2[CC1, CC2] = (_1, _2)

    def build(_1: CC1, _2: CC2): T

    override def build(contents: Product2[CC1, CC2]): T = build(contents._1, contents._2)
  }

  object Leaf2 {
    def unapply[CC1, CC2](arg: Leaf2[CC1, CC2, _]): Option[(CC1, CC2)] = Some(arg._1, arg._2)
  }


  /**
    * Node with extra member label, respresenting a node's Label. [[pattern.Application]] that extends this trait.
    *
    * @tparam L Type of Label.
    */
  sealed trait LabeledNode[L, T <: AST[T]] extends Node[T] with Product1[L] {
    def build(_1: L, args: Seq[T]): T

    override def build(children: Seq[T]): T = build(_1, children)
  }

  object LabeledNode {
    def unapply[L](arg: LabeledNode[L, _]): Option[(L, Seq[_])] = Some(arg._1, arg.args)
  }


  /**
    * A Node with empty list of Patterns as its args list. [[pattern.Top]], [[pattern.Bottom]] extend this trait.
    */
  sealed trait Node0[T <: AST[T]] extends Node[T] {
    override def args = Seq.empty[T]

    def build(): T

    override def build(children: Seq[T]): T = {
      assert(children.isEmpty)
      build()
    }
  }

  object Node0 {
    def unapply(arg: Node0[_]): Boolean = true
  }


  /**
    * A Node with a single pattern in its args list. Extended by [[pattern.Next]], [[pattern.Not]].
    */
  sealed trait Node1[T <: AST[T]] extends Node[T] with Product1[T] {
    override def args = Seq(_1)

    def build(_1: T): T

    override def build(children: Seq[T]): T = {
      assert(children.size == 1)
      build(children.head)
    }
  }

  object Node1 {
    def unapply(arg: Node1[_]): Option[_] = Some(arg._1)
  }


  /**
    * A Node with two Patterns in its args list. Extended by [[pattern.Or]], [[pattern.And]], [[pattern.Implies]], [[pattern.Equals]], [[pattern.Rewrite]].
    */
  sealed trait Node2[T <: AST[T]] extends Node[T] with Product2[T, T] {
    def build(_1: T, _2: T): T

    override def args = Seq(_1, _2)

    override def build(children: Seq[T]): T = {
      assert(children.size == 2)
      build(children.head, children(1))
    }
  }

  object Node2 {
    def unapply(arg: Node2[_]): Option[(_, _)] = Some(arg._1, arg._2)
  }


  /**
    * Extends [[Node2]], and only allows [[pattern.Variable]] as the first element, and [[pattern.Pattern]] in its args list. Extended by [[pattern.Exists]], [[pattern.ForAll]].
    *
    * TODO: An extension of Binder Node may be provided in the future, to allow user defined symbols to have binder-like behavior.
    *       These symbols may allow multiple arguments, with the variable bound in some, but not all of them.
    */
  sealed trait BinderNode[V <: T, T <: AST[T]] extends Node[T] with Product2[V, T] {
    def build(_1: V, _2: T): T

    override def args = Seq(_1, _2)

    override def build(children: Seq[T]): T = {
      assert(children.size == 2)
      build(children.head.asInstanceOf[V], children(1))
    }
  }

  object BinderNode {
    def unapply(arg: BinderNode[_, _]): Option[(_, _)] = Some(arg._1, arg._2)
  }

}

/**
  * Provides all Pattern types, and destructors for matching on [[pattern.Pattern]]s.
  *
  * Sample Usage -
  * {{{
  *   def filterAndConstructs(patterns: Seq[Pattern]): Seq[Pattern] = patterns collect {
  *     case a@And(x: Pattern, y: Pattern) => a
  *   }
  * }}}
  *
  */
object pattern {

  import tree._

  /* ML Pattern Type */
  sealed trait Pattern extends AST[Pattern]


  /**
    * Matching Logic Variable.
    *
    * Provides (Implementations for members)
    *    - contents of type Product2[Name, Sort].
    *
    * Requires (Implementation for members)
    *    - _1 of type [[Name]].
    *    - _2 of type [[Sort]].
    *    - build method taking arguments ([[Name]], [[Sort]]) and returning [[Variable]].
    */
  trait Variable extends Pattern with Leaf2[Name, Sort, Pattern] {
    def build(_1: Name, _2: Sort): Variable
  }

  object Variable {
    def unapply(arg: Variable): Option[(Name, Sort)] = Some(arg._1, arg._2)
  }


  case class Symbol(str: String)

  case class Sort(str: String)

  type Value = String

  type Name = String


  /**
    * Matching Logic DomainValue.
    *
    * Provides (Implementations for members)
    *    - contents of type Product2[Symbol, Value].
    *
    * Requires (Implementation for members)
    *    - _1 of type [[Symbol]].
    *    - _2 of type [[Value]].
    *    - build method taking arguments ([[Symbol]], [[Value]]) and returning [[DomainValue]].
    */
  trait DomainValue extends Pattern with Leaf2[Symbol, Value, Pattern] {
    def build(_1: Symbol, _2: Value): DomainValue
  }

  object DomainValue {
    def unapply(arg: DomainValue): Option[(Symbol, Value)] = Some(arg._1, arg._2)
  }


  /**
    * Matching Logic Top.
    *
    * Provides (Implementation for members)
    *    - args, an empty list.
    *
    * Requires (Implementation for members)
    *    - build method taking arguments () and returning [[Top]].
    */
  trait Top extends Pattern with Node0[Pattern] {
    override def build(): Top
  }

  object Top {
    def unapply(arg: Top): Boolean = true
  }


  /**
    * Matching Logic Bottom.
    *
    * Provides (Implementation for members)
    *    - args, an empty list.
    *
    * Requires (Implementation for members)
    *    - build method taking arguments () and returning [[Bottom]].
    */
  trait Bottom extends Pattern with Node0[Pattern] {
    override def build(): Bottom
  }

  object Bottom {
    def unapply(arg: Bottom): Boolean = true
  }


  /**
    * Matching Logic And.
    *
    * Provides (Implementation for members)
    *    - args, a list containing two [[Pattern]]s.
    *
    * Requires (Implementation for members)
    *    - _1 of type [[Pattern]].
    *    - _2 of type [[Pattern]].
    *    - build method taking arguments ([[Pattern]], [[Pattern]]) and returning [[And]].
    */
  trait And extends Pattern with Node2[Pattern] {
    override def build(_1: Pattern, _2: Pattern): And
  }

  object And {
    def unapply(arg: And): Option[(Pattern, Pattern)] = Some(arg._1, arg._2)
  }


  /**
    * Matching Logic Or.
    *
    * Provides (Implementation for members)
    *    - args, a list containing two [[Pattern]]s.
    *
    * Requires (Implementation for members)
    *    - _1 of type [[Pattern]].
    *    - _2 of type [[Pattern]].
    *    - build method taking arguments ([[Pattern]], [[Pattern]]) and returning [[Or]].
    */
  trait Or extends Pattern with Node2[Pattern] {
    override def build(_1: Pattern, _2: Pattern): Or
  }

  object Or {
    def unapply(arg: Or): Option[(Pattern, Pattern)] = Some(arg._1, arg._2)
  }


  /**
    * Matching Logic Not.
    *
    * Provides (Implementation for members)
    *    - args, a list containing one [[Pattern]].
    *
    * Requires (Implementation for members)
    *    - _1 of type [[Pattern]].
    *    - build method taking argument ([[Pattern]]) and returning [[Not]].
    */
  trait Not extends Pattern with Node1[Pattern] {
    override def build(_1: Pattern): Not
  }

  object Not {
    def unapply(arg: Not): Option[Pattern] = Some(arg._1)
  }


  /**
    * Matching Logic Symbol Application.
    *
    * Requires (Implementation for members)
    *    - _1 of type [[Symbol]], representing symbol from Matching Logic Algebra.
    *    - args of type Seq[Pattern].
    *    - build method taking arguments ([[Symbol]], Seq[Pattern]) and returning [[Application]].
    */
  trait Application extends Pattern with LabeledNode[Symbol, Pattern] {
    override def build(_1: Symbol, args: Seq[Pattern]): Application
  }

  object Application {
    def unapply(arg: Application): Option[(Symbol, Seq[Pattern])] = Some(arg._1, arg.args)
  }


  /**
    * Matching Logic Implies.
    *
    * Provides (Implementation for members)
    *    - args, a list containing two [[Pattern]]s.
    *
    * Requires (Implementation for members)
    *    - _1 of type [[Pattern]].
    *    - _2 of type [[Pattern]].
    *    - build method taking arguments ([[Pattern]], [[Pattern]]) and returning [[Implies]].
    */
  trait Implies extends Pattern with Node2[Pattern] {
    override def build(_1: Pattern, _2: Pattern): Implies
  }

  object Implies {
    def unapply(arg: Implies): Option[(Pattern, Pattern)] = Some(arg._1, arg._2)
  }


  /**
    * Matching Logic Existential Quantifier.
    *
    * Provides (Implementation for members)
    *    - args, a list containing a [[Variable]] and a [[Pattern]].
    *
    * Requires (Implementation for members)
    *    - _1 of type [[Variable]].
    *    - _2 of type [[Pattern]].
    *    - build method taking arguments ([[Variable]], [[Pattern]]) and returning [[Exists]].
    */
  trait Exists extends Pattern with BinderNode[Variable, Pattern] {
    def build(_1: Variable, _2: Pattern): Exists
  }

  object Exists {
    def unapply(arg: Exists): Option[(Variable, Pattern)] = Some(arg._1, arg._2)
  }


  /**
    * Matching Logic ForAll Quantifier.
    *
    * Provides (Implementation for members)
    *    - args, a list containing a [[Variable]] and a [[Pattern]].
    *
    * Requires (Implementation for members)
    *    - _1 of type [[Variable]].
    *    - _2 of type [[Pattern]].
    *    - build method taking arguments ([[Variable]], [[Pattern]]) and returning [[ForAll]].
    */
  trait ForAll extends Pattern with BinderNode[Variable, Pattern] {
    def build(_1: Variable, _2: Pattern): ForAll
  }

  object ForAll {
    def unapply(arg: ForAll): Option[(Variable, Pattern)] = Some(arg._1, arg._2)
  }


  /**
    * Matching Logic Next.
    *
    * Provides (Implementation for members)
    *    - args, a list containing one [[Pattern]].
    *
    * Requires (Implementation for members)
    *    - _1 of type [[Pattern]].
    *    - build method taking argument ([[Pattern]]) and returning [[Next]].
    */
  trait Next extends Pattern with Node1[Pattern] {
    override def build(_1: Pattern): Next
  }

  object Next {
    def unapply(arg: Next): Option[Pattern] = Some(arg._1)
  }


  /**
    * Matching Logic Rewrite.
    *
    * Provides (Implementation for members)
    *    - args, a list containing two [[Pattern]]s.
    *
    * Requires (Implementation for members)
    *    - _1 of type [[Pattern]].
    *    - _2 of type [[Pattern]].
    *    - build method taking arguments ([[Pattern]], [[Pattern]]) and returning [[Rewrite]].
    */
  trait Rewrite extends Pattern with Node2[Pattern] {
    override def build(_1: Pattern, _2: Pattern): Rewrite
  }

  object Rewrite {
    def unapply(arg: Rewrite): Option[(Pattern, Pattern)] = Some(arg._1, arg._2)
  }


  /**
    * Matching Logic Equals.
    *
    * Provides (Implementation for members)
    *   - args, a list containing two [[Pattern]]s.
    *
    * Requires (Implementation for members)
    *   - _1 of type [[Pattern]].
    *   - _2 of type [[Pattern]].
    *   - build method taking arguments ([[Pattern]], [[Pattern]]) and returning [[Equals]].
    */
  trait Equals extends Pattern with Node2[Pattern] {
    override def build(_1: Pattern, _2: Pattern): Equals
  }

  object Equals {
    def unapply(arg: Equals): Option[(Pattern, Pattern)] = Some(arg._1, arg._2)
  }


}

object outer {

  import org.kframework.kore.interfaces.{pattern => p}

  type Attributes = Seq[p.Pattern]

  /**
    * Trait That is Extended by Every Outer Construct.
    */
  trait Outer {
    val att: Attributes
    val sorts: Set[p.Sort]
    val symbols: Set[p.Symbol]
    val sentences: Seq[Sentence]

    def onAttributes(f: p.Pattern => p.Pattern): Outer

    def getBySymbol(key: p.Symbol): Seq[Seq[p.Pattern]] = att.collect({ case p.Application(`key`, args) => args })
  }


  /** Kore Definition.
    *
    * Provides (Implementation for members)
    *   - sorts, a set of all [[pattern.Sort]]s, obtained recursively.
    *   - symbols, a set of all [[pattern.Symbol]]s, obtained recursively.
    *   - sentences, a set of all [[Sentence]]s, obtained recursively.
    *
    *   Requires (Implementation for members)
    *   - modules, a Seq of [[Module]]s
   */
  trait Definition extends Outer {
    val modules: Seq[Module]

    // Derived operations
    lazy val sorts: Set[p.Sort] = modules.flatMap(_.sorts).toSet
    lazy val symbols: Set[p.Symbol] = modules.flatMap(_.symbols).toSet
    lazy val sentences: Seq[Sentence] = modules.flatMap(_.sentences)

    override def onAttributes(f: p.Pattern => p.Pattern): Definition
  }

  object Definition {
    def unapply(arg: Definition): Option[(Seq[Module], Attributes)] = Some(arg.modules, arg.att)
  }

  /** Kore Module.
    *
    * Provides (Implementation for members)
    *   - sorts, a set of all [[pattern.Sort]]s, obtained recursively.
    *   - symbols, a set of all [[pattern.Symbol]]s, obtained recursively.
    *
    *   Requires (Implementation for members)
    *   - modules, a Seq of [[Sentence]]s
    */
  trait Module extends Outer {
    val name: p.Name
    val sentences: Seq[Sentence]

    // Derived operations
    lazy val sorts: Set[p.Sort] = sentences.flatMap(_.sorts).toSet
    lazy val symbols: Set[p.Symbol] = sentences.flatMap(_.symbols).toSet

    override def onAttributes(f: p.Pattern => p.Pattern): Module
  }

  object Module {
    def unapply(arg: Module): Option[(p.Name, Seq[Sentence], Attributes)] = Some(arg.name, arg.sentences, arg.att)
  }


  /** Kore Sentence. Extended by [[Import]], [[SymbolDeclaration]], [[SortDeclaration]], [[Axiom]], and [[Rule]]
    */
  trait Sentence extends Outer {
    lazy val sorts: Set[p.Sort] = Set.empty
    lazy val symbols: Set[p.Symbol] = Set.empty
    val sentences: Seq[Sentence] = Seq(this)

    override def onAttributes(f: p.Pattern => p.Pattern): Sentence
  }


  /** Kore Import.
    *
    *   Requires (Implementation for members)
    *   - name, of type [[pattern.Name]]
    */
  trait Import extends Sentence {
    val name: p.Name

    override def onAttributes(f: p.Pattern => p.Pattern): Import
  }

  object Import {
    def unapply(arg: Import): Option[(p.Name, Attributes)] = Some(arg.name, arg.att)
  }


  /** Kore Sort Declaration.
    *
    *  Requires (Implementation for members)
    *  - sort, of type [[pattern.Sort]]
    */
  trait SortDeclaration extends Sentence {
    val sort: p.Sort

    override lazy val sorts = Set(sort)

    override def onAttributes(f: p.Pattern => p.Pattern): SortDeclaration
  }

  object SortDeclaration {
    def unapply(arg: SortDeclaration): Option[(p.Sort, Attributes)] = Some(arg.sort, arg.att)
  }


  /** Kore Sort Declaration.
    *
    *  Requires (Implementation for members)
    *  - sort, of type [[pattern.Sort]]
    */
  trait SymbolDeclaration extends Sentence {
    val sort: p.Sort
    val symbol: p.Symbol
    val args: Seq[p.Sort]

    override lazy val sorts = Set(sort)
    override lazy val symbols = Set(symbol)

    override def onAttributes(f: p.Pattern => p.Pattern): SymbolDeclaration
  }

  object SymbolDeclaration {
    def unapply(arg: SymbolDeclaration): Option[(p.Sort, p.Symbol, Seq[p.Sort], Attributes)] = Some(arg.sort, arg.symbol, arg.args, arg.att)
  }


  /** Kore Rule
    *
    * Requires (Implementation for members)
    *   - pattern, the rule's [[pattern.Pattern]]
    */
  trait Rule extends Sentence {
    val pattern: p.Pattern

    override def onAttributes(f: p.Pattern => p.Pattern): Rule
  }

  object Rule {
    def unapply(arg: Rule): Option[(p.Pattern, Attributes)] = Some(arg.pattern, arg.att)
  }

  /** Kore Axiom
    *
    * Requires (Implementation for members)
    *   - Axiom, the axiom's [[pattern.Pattern]]
    */
  trait Axiom extends Sentence {
    val pattern: p.Pattern

    override def onAttributes(f: p.Pattern => p.Pattern): Axiom
  }

  object Axiom {
    def unapply(arg: Axiom): Option[(p.Pattern, Attributes)] = Some(arg.pattern, arg.att)
  }

}

object builders {

  /**
    * Builder type that allows building types in [[outer]].
    *
    * Sample Usage -
    *
    * Given a concrete implementation of [[builders.OuterBuilders]], one can create new outer types-
    * {{{
    *   /* Given Concrete Implementation */
    *   val builder: OuterBuilders = ConcreteBuilders
    *   /* A SymbolDeclaration can made in the following way */
    *   val symbolDec: SymbolDeclaration = builder.SymbolDeclaration(Sort("Exp"), Symbol("Plus"), Seq(Sort("Int"), Sort("Int")), Seq.empty())
    * }}}
    *
    */

  /**
    * The Builders trait has one method for every construct in [[outer]], with the same name.
    * Implementations are expected to implement the methods, allowing tools to
    * build patterns in an implementation independent manner.
    */
  trait OuterBuilders {
    import outer._
    import pattern.{Name, Sort, Symbol, Pattern}

    def Definition(modules: Seq[Module], att: Attributes): Definition
    def Module(name: Name, sentences: Seq[Sentence], att: Attributes): Module
    def Import(name: Name, att: Attributes): Import
    def SortDeclaration(sort: Sort, att: Attributes): SortDeclaration
    def SymbolDeclaration(sort: Sort, symbol: Symbol, args: Seq[Sort], att: Attributes): SymbolDeclaration
    def Rule(pattern: Pattern, att: Attributes): Rule
    def Axiom(pattern: Pattern, att: Attributes): Axiom
  }

  /**
    * Provides a Builder type that allows building Pattern types in [[pattern]].
    *
    * Sample Usage -
    *
    * Given a concrete implementation of [[builders.PatternBuilders]], one can create new patters -
    * {{{
    *   /* Given Concrete Implementation */
    *   val builder: Builders = ConcreteBuilders
    *   /* A Pattern Or(X:Int, Y:Int) can be constructed in the following way */
    *   val or: Or = builder.Or(builder.Variable("X", Sort("Int")), builder."Y", Sort("Int"))
    * }}}
    *
    */

  /**
    * The Builders trait has one method for every [[pattern.Pattern]], with the same name.
    * Implementations are expected to implement the methods, allowing tools to
    * build patterns in an implementation independent manner.
    */

  trait PatternBuilders {
    import pattern._

    def Variable(_1: Name, _2: Sort): Variable
    def DomainValue(_1: Symbol, _2: Value): DomainValue
    def Top(): Top
    def Bottom(): Bottom
    def Not(_1: Pattern): Not
    def Next(_1: Pattern): Next
    def And(_1: Pattern, _2: Pattern): And
    def Or(_1: Pattern, _2: Pattern): Or
    def Implies(_1: Pattern, _2: Pattern): Implies
    def Equals(_1: Pattern, _2: Pattern): Equals
    def Exists(_1: Variable, _2: Pattern): Exists
    def ForAll(_1: Variable, _2: Pattern): ForAll
    def Rewrite(_1: Pattern, _2: Pattern): Rewrite
    def Application(_1: Symbol, args: Seq[Pattern]): Application
  }

}