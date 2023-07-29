#include "colors.inc"
#include "pokeballDilate1.inc"
background { color Cyan }
camera {
	orthographic
	location -2.5 * #dir + <0, 0, -5>
	look_at  <0, 0, 0>
	right x * 2
	up y * 2
}
#declare s = sphere {
	<0, 0, 0>, 2
}
object {
  #cl0
	texture {
	  pigment { color White }
	}
	finish {
	  ambient 0.4
	}
}
object {
  #cl1
	texture {
	  pigment { color Red }
	}
	finish {
	  ambient 0.4
	}
}
object {
  #cl2
	texture {
	  pigment { color Yellow }
	}
	finish {
	  ambient 0.4
	}
}
object {
  #cl3
	texture {
	  pigment { color Magenta }
	}
	finish {
	  ambient 0.4
	}
}
object {
  #cl4
	texture {
	  pigment { color Green }
	}
	finish {
	  ambient 0.4
	}
}
object {
  #cl5
	texture { pigment { color Blue } }
	finish {
	  ambient 0.4
	}
}
object {
  #cl6
	texture { pigment { color Violet } }
	finish {
	  ambient 0.4
	}
}

object {
  #cl7
	texture { pigment { color Orange } }
	finish {
	  ambient 0.4
	}
}


light_source { -8 * dir + <2, 1, 0> color White}
