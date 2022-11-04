import fpinscala.laziness.Stream

Stream(1, 2, 3).toList

Stream(1, 2, 3).take(3).toList

Stream.constant(5).take(4).toList

Stream.from(5).take(4).toList

Stream.fibs.take(5).toList

Stream.onesViaUnfold.take(5).toList

Stream.constantViaUnfold("hi").take(5).toList

Stream.fromViaUnfold(42).take(5).toList

Stream.fibsViaUnfold.take(5).toList

Stream.fibsViaUnfold.takeViaUnfold(5).toList

Stream.fibsViaUnfold.takeWhileViaUnfold(_ < 3).toList