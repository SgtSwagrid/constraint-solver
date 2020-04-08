class ConstraintSolver[T](variables: Seq[Variable[T]],
                          constraints: Seq[Constraint[T]],
                          alphabet: Seq[T] = Seq[T]()) {

  private val indexed = variables.map{v =>
    v -> constraints.filter{_.variables.contains(v)}
  }.toMap

  def solve(values: Map[Variable[T], Seq[T]] =
            variables.map{_ -> alphabet}.toMap,
            unassigned: Seq[Variable[T]] = variables):
  Option[Map[Variable[T], T]] =

    if(unassigned.exists{values(_).size == 0})
      Option.empty

    else if(unassigned.isEmpty)
      Option.apply(values.mapValues{_(0)})

    else {

      val sorted = unassigned.sortBy{values(_).size}
      val variable = sorted.head

      val vals = values(variable).sortBy{ v =>
        -apply(values, variable, v, indexed(variable))
          .values.map{_.size}.sum
      }

      vals.foldLeft(Option.empty[Map[Variable[T], T]]) {
        (result, value) => result match {
          case Some(_) => result
          case None => solve(apply(values, variable,
            value, indexed(variable)), sorted.tail)
        }
      }
    }

  private def apply(values: Map[Variable[T], Seq[T]],
                    variable: Variable[T],
                    value: T,
                    constraints: Seq[Constraint[T]]):
  Map[Variable[T], Seq[T]] =

    constraints.foldLeft(values){(values, constraint) =>
      constraint.apply(values, variable, value)
    } + (variable -> Seq(value))
}

class Variable[T](val name: String) {
  override def toString: String = name
}

abstract class Constraint[T](val variables: Seq[Variable[T]]) {

  def apply(values: Map[Variable[T], Seq[T]],
            variable: Variable[T],
            value: T):
  Map[Variable[T], Seq[T]]
}

class Same[T](variables: Variable[T]*)
  extends Constraint[T](variables) {

  override def apply(values: Map[Variable[T], Seq[T]],
                     variable: Variable[T],
                     value: T):
  Map[Variable[T], Seq[T]] =

    values.transform{(v, vals) =>
      if(variables.contains(v))
        vals.filter{_ == value}
      else vals
    }
}

class Different[T](variables: Variable[T]*)
    extends Constraint[T](variables) {

  override def apply(values: Map[Variable[T], Seq[T]],
                     variable: Variable[T],
                     value: T):
  Map[Variable[T], Seq[T]] =

    values.transform{(v, vals) =>
      if(variables.contains(v))
        vals.filter{_ != value}
      else vals
    }
}
