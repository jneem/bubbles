#include "colors.inc"
#include "degenerate.inc"
background { color Cyan }
camera {
	orthographic
	location -2.5 * #dir
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
	  ambient 0.3
	}
}
object {
  #cl1
	texture {
	  pigment {
	    checker Red, White
		scale 0.01
	  }
	}
	finish {
	  ambient 0.3
	}
}
object {
  #cl2
	texture {
	  pigment { color Green }
	}
	finish {
	  ambient 0.3
	}
}
object {
  #cl3
	texture {
	  pigment {
	    hexagon Blue, LightBlue, Blue
		scale 0.002
	  }
	}
	finish {
	  ambient 0.3
	}
}
object {
  #cl4
	texture {
	  pigment {
	    hexagon Yellow, Yellow, Orange
		scale 0.002
	  }
	}
	finish {
	  ambient 0.3
	}
}
light_source { -8 * dir + <2, 1, 0> color White}
