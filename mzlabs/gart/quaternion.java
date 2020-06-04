
// $Header: C:\\JMount\\geneticArt/RCS/quaternion.java,v 1.2 1999/06/03 00:54:26 jmount Exp jmount $

public class quaternion {
    private double e = 0.0; // the data
    private double i = 0.0;
    private double j = 0.0;
    private double k = 0.0;  

    public quaternion() {
    }

    public double normsq() {
	return e*e + i*i + j*j + k*k;
    }

    public double getE() {
	return e;
    }

    public double getI() {
	return i;
    }

    public double getJ() {
	return j;
    }

    public double getK() {
	return k;
    }

    // basic quaternion ops that can have this equal to inputs

    // sets this fields to a
    public void set(quaternion a) {
	e = a.e;
	i = a.i;
	j = a.j;
	k = a.k;
    }

    public void qplus(quaternion a, quaternion b) {
	e = a.e + b.e;
	i = a.i + b.i;
	j = a.j + b.j;
	k = a.k + b.k;
    }

    public void qneg(quaternion a) {
	e = -a.e;
	i = -a.i;
	j = -a.j;
	k = -a.k;
    }

    public void qsub(quaternion a, quaternion b) {
	e = a.e - b.e;
	i = a.i - b.i;
	j = a.j - b.j;
	k = a.k - b.k;
    }

    public void qinv(quaternion a) {
	double d = a.normsq();
	if(d<=1.0e-9) {
	    // arbitrary behavior for dividing by 0 or near 0
	    e = 0.0;
	    i = 0.0;
	    j = 0.0;
	    k = 0.0;
	} else {
	    d = 1.0/d;
	    e = a.e*d;
	    i = -a.i*d;
	    j = -a.j*d;
	    k = -a.k*d;
	}
    }

    public void qconj(quaternion a) {
	e = a.e;
	i = -a.i;
	j = -a.j;
	k = -a.k;
    }


    public void set(double e_in, double i_in, double j_in, double k_in) {
	e = e_in;
	i = i_in;
	j = j_in;
	k = k_in;
    }

    public void qmult(quaternion a, quaternion b) {
	set(a.e*b.e - a.i*b.i - a.j*b.j - a.k*b.k,
	    a.e*b.i + a.i*b.e + a.j*b.k - a.k*b.j,
	    a.e*b.j - a.i*b.k + a.j*b.e + a.k*b.i,
	    a.e*b.k + a.i*b.j - a.j*b.i + a.k*b.e);
    }

    public void qdiv(quaternion a, quaternion b) {
	quaternion t1 = new quaternion();
	t1.qinv(b);
	qmult(a,t1);
    }

    // the unit quaternions under multiplication form an important group
    // ( isomorphic to SU(2) (2x2 complex matracies with determinant 1) )
    // normalizing quaternions injects us into the group
    public void qnorm(quaternion a) {
	double d = a.normsq();
	if(d<=1.0e-9) {
	    e = 0.0;
	    i = 0.0;
	    j = 0.0;
	    k = 0.0;
	} else {
	    d = 1.0/Math.sqrt(d);
	    e = a.e * d;
	    i = a.i * d;
	    j = a.j * d;
	    k = a.k * d;
	}
    }

    // the projective version of the unit quaternions (identifying a with -a)
    // is another important group isomophic to PSU(2) qnormp injects into this
    public void qnormp(quaternion a) {
	qnorm(a);
	if(e!=0.0) {
	    if(e<0.0) {
		qneg(this);
	    }
	    return;
	} 
	if(i!=0.0) {
	    if(i<0.0) {
		qneg(this);
	    }
	    return;
	} 
	if(j!=0.0) {
	    if(j<0.0) {
		qneg(this);
	    }
	    return;
	} 
	if(k!=0.0) {
	    if(k<0.0) {
		qneg(this);
	    }
	    return;
	}
    }

    // a coordinate independent rounding to the integer 
    // subring of the quaternions
    public void qfloor(quaternion a) {
	e = Math.floor(a.e);
	i = Math.floor(a.i);
	j = Math.floor(a.j);
	k = Math.floor(a.k);
    }

    // constant yielding ops

    public void qc1() {
	e = 1.0;
	i = 0.0;
	j = 0.0;
	k = 0.0;
    }

    public void qc2() {
	e = 0.0;
	i = 1.0;
	j = 0.0;
	k = 0.0;
    }

    public void qc3() {
	e = 0.0;
	i = 0.0;
	j = 1.0;
	k = 0.0;
    }

    public void qc4() {
	e = 0.0;
	i = 0.0;
	j = 0.0;
	k = 1.0;
    }

    public void qc5() {
	e = 1.61803398875;  // golden ratio
	i = 0.0;
	j = 0.0;
	k = 0.0;
    }

    // varible entry points

    public void qcx(double x, double y) {
	e = x;
	i = 0.0;
	j = 0.0;
	k = 0.0;
    }

    public void qcy(double x, double y) {
	e = y;
	i = 0.0;
	j = 0.0;
	k = 0.0;
    }

    // extras to help close tree and make eqns reactive
    public void qcx1(double x, double y) {
	e = x;
	i = 0.0;
	j = 0.0;
	k = 1.0;
    }

    public void qcy1(double x, double y) {
	e = y;
	i = 0.0;
	j = 0.0;
	k = 1.0;
    }

    public void qcx2(double x, double y) {
	e = x;
	i = 1.0;
	j = 0.0;
	k = 0.0;
    }

    public void qcy2(double x, double y) {
	e = y;
	i = 0.0;
	j = 1.0;
	k = 0.0;
    }

    public void qcxy(double x, double y) {
	e = x;
	i = y;
	j = 0.0;
	k = 0.0;
    }

    public void qcxy2(double x, double y) {
	e = x;
	i = y;
	j = x;
	k = y;
    }

    // coordinate independent functions
    // not interesting from the quaternion point of view- but since we
    // derive colors from the coordinates independently these functions should
    // have some visual value.

    public void qisin(quaternion a) {
	e = Math.sin(a.e);
	i = Math.sin(a.i);
	j = Math.sin(a.j);
	k = Math.sin(a.k);
    }

    public void qilog(quaternion a) {
	e = Math.log(Math.max(a.e,0.000001));
	i = Math.log(Math.max(a.i,0.000001));
	j = Math.log(Math.max(a.j,0.000001));
	k = Math.log(Math.max(a.k,0.000001));
    }

    public void qiexp(quaternion a) {
	e = Math.exp(Math.min(Math.max(a.e,-30.0),30.0));
	i = Math.exp(Math.min(Math.max(a.i,-30.0),30.0));
	j = Math.exp(Math.min(Math.max(a.j,-30.0),30.0));
	k = Math.exp(Math.min(Math.max(a.k,-30.0),30.0));
    }

    public void qimin(quaternion a, quaternion b) {
	e = Math.min(a.e,b.e);
	i = Math.min(a.i,b.i);
	j = Math.min(a.j,b.j);
	k = Math.min(a.k,b.k);
    }

    public void qimax(quaternion a, quaternion b) {
	e = Math.max(a.e,b.e);
	i = Math.max(a.i,b.i);
	j = Math.max(a.j,b.j);
	k = Math.max(a.k,b.k);
    }


    // shift coordinates around (not a Quaternion automorphism)

    public void qrl(quaternion a) {
	double t = a.e;
	e = a.i;
	i = a.j;
	j = a.k;
	k = t;
    }

    public void qrr(quaternion a) {
	double t = a.k;
	k = a.j;
	j = a.i;
	i = a.e;
	e = t;
    }


    // more exotic ops (can not have this equal to an argument)

    // all R-algrebra automorphisms of H (the quaternions) are of the form
    // qaut1 by corollary of a theorem of Cayley's
    public void qaut1(quaternion a, quaternion b) {
	quaternion t2 = new quaternion();
	t2.qdiv(b,a);
	qmult(a,t2);
    }

    // not an automorphism- but close
    public void qaut2(quaternion a, quaternion b) {
	quaternion t3 = new quaternion();
	t3.qconj(b);
	qaut1(a,t3);
    }

    // exponential map -
    //    using first few terms of power series as aproximation
    public void qexp(quaternion ai) {
	quaternion p = new quaternion();
	quaternion a = new quaternion();
	quaternion t3 = new quaternion();
	a.set(ai);
	// don't touch big arguments
	while(a.normsq()>=900.0) {
	    a.e /= 10.0;
	    a.i /= 10.0;
	    a.j /= 10.0;
	    a.k /= 10.0;
	}
	set(a);
	p.set(a);
	e += 1.0; // zero power term
	for(int ti=2;ti<10;++ti) {
	    double d;
       
	    d = 1.0/(double)ti;
	    t3.qmult(p,a);
	    p.e = t3.e*d;
	    p.i = t3.i*d;
	    p.j = t3.j*d;
	    p.k = t3.k*d;
	    t3.set(this);
	    qplus(t3,p);
	}
    }


    // there is a polar repersentation of quaternions: for every quaternion a
    // there is an imaginvar quaternion u s.t. a = |a| exp(u).
    // finding u looks hard.
    // we can use the fact the u.u = -|u|^2 to break exp(u) into two real
    // series:  sum_{k=0}^{\infty} (-|u|^2)^k / (2 k)! +
    // u (sum_{k=0}^{\infty} (-|u|^2)^k / ( 2 k +1)!
    // but this looks like a mess


    // by Hamilton's theorem for every ortogonal mappinger f: Im H->Im H there
    // is a unit quaternion a s.t. f(b) = a b bar(a) or f(b) = - a b bar(a)
    // qorth1 and qorth2 implement these maps
    public void qorth1(quaternion a, quaternion b) {
	quaternion t1 = new quaternion();
	quaternion t2 = new quaternion();
	quaternion t3 = new quaternion();
	t1.qnorm(a);
	t2.qconj(t1);
	t3.qmult(b,t2);
	qmult(t1,t3);
    }

    public void qorth2(quaternion a, quaternion b) {
	qorth1(a,b);
	qneg(this);
    }

    // mod (or remainder) over the quaternions in analogy to traditional mod
    public void qmod(quaternion a,quaternion b) {
	qdiv(a,b);
	qfloor(this);
	quaternion t1 = new quaternion();
	t1.qmult(this,b);
	qsub(a,t1);
    }
}


