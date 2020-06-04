
// $Header: C:\\JMount\\geneticArt/RCS/qtree.java,v 1.3 1999/05/29 22:57:21 jmount Exp $

import java.awt.*;   // needed for color/image defs

public class qtree implements Cloneable {
    // minor class to hold operator definitions
    public static abstract class qop {
	public abstract String pname();
	public abstract int degree();
	public boolean isconst() { return false; }
	public abstract void 
	    dop(qtree n, quaternion a, quaternion b, double x, double y);
    };

    // long list of operators
    public static class qop_qplus extends qop {
	public String pname() { return "+"; }
	public int degree() { return 2; }
	public void dop(qtree n, quaternion a, quaternion b, 
			double x, double y)
	{
	    n._v.qplus(a,b);
	}
    }
    
    public static class qop_qsub extends qop {
	public String pname() { return "-"; }
	public int degree() { return 2; }
	public void dop(qtree n, quaternion a, quaternion b, 
			double x, double y)
	{
	    n._v.qsub(a,b);
	}
    }
    
    public static class qop_qmult extends qop {
	public String pname() { return "*"; }
	public int degree() { return 2; }
	public void dop(qtree n, quaternion a, quaternion b, 
			double x, double y)
	{
	    n._v.qmult(a,b);
	}
    }
    
    public static class qop_qinv extends qop {
	public String pname() { return "inv"; }
	public int degree() { return 1; }
	public void dop(qtree n, quaternion a, quaternion b, 
			double x, double y)
	{
	    n._v.qinv(a);
	}
    }
    
    public static class qop_qdiv extends qop {
	public String pname() { return "/"; }
	public int degree() { return 2; }
	public void dop(qtree n, quaternion a, quaternion b, 
			double x, double y)
	{
	    n._v.qdiv(a,b);
	}
    }
    
    public static class qop_qconj extends qop {
	public String pname() { return "conj"; }
	public int degree() { return 1; }
	public void dop(qtree n, quaternion a, quaternion b, 
			double x, double y)
	{
	    n._v.qconj(a);
	}
    }
    
    public static class qop_qaut1 extends qop {
	public String pname() { return "A1"; }
	public int degree() { return 2; }
	public void dop(qtree n, quaternion a, quaternion b, 
			double x, double y)
	{
	    n._v.qaut1(a,b);
	}
    }
    
    public static class qop_qaut2 extends qop {
	public String pname() { return "A2"; }
	public int degree() { return 2; }
	public void dop(qtree n, quaternion a, quaternion b, 
			double x, double y)
	{
	    n._v.qaut2(a,b);
	}
    }
    
    public static class qop_qexp extends qop {
	public String pname() { return "exp"; }
	public int degree() { return 1; }
	public void dop(qtree n, quaternion a, quaternion b, 
			double x, double y)
	{
	    n._v.qexp(a);
	}
    }
    
    public static class qop_qfloor extends qop {
	public String pname() { return "floor"; }
	public int degree() { return 1; }
	public void dop(qtree n, quaternion a, quaternion b, 
			double x, double y)
	{
	    n._v.qfloor(a);
	}
    }
    
    public static class qop_qmod extends qop {
	public String pname() { return "mod"; }
	public int degree() { return 2; }
	public void dop(qtree n, quaternion a, quaternion b, 
			double x, double y)
	{
	    n._v.qmod(a,b);
	}
    }
    
    public static class qop_qnorm extends qop {
	public String pname() { return "normalize"; }
	public int degree() { return 1; }
	public void dop(qtree n, quaternion a, quaternion b, 
			double x, double y)
	{
	    n._v.qnorm(a);
	}
    }
    
    public static class qop_qnormp extends qop {
	public String pname() { return "normp"; }
	public int degree() { return 1; }
	public void dop(qtree n, quaternion a, quaternion b, 
			double x, double y)
	{
	    n._v.qnormp(a);
	}
    }
    
    public static class qop_qorth1 extends qop {
	public String pname() { return "orth1"; }
	public int degree() { return 2; }
	public void dop(qtree n, quaternion a, quaternion b, 
			double x, double y)
	{
	    n._v.qorth1(a,b);
	}
    }
    
    public static class qop_qorth2 extends qop {
	public String pname() { return "orth2"; }
	public int degree() { return 2; }
	public void dop(qtree n, quaternion a, quaternion b, 
			double x, double y)
	{
	    n._v.qorth2(a,b);
	}
    }
    
    public static class qop_qc1 extends qop {
	public String pname() { return "1"; }
	public int degree() { return 0; }
	public boolean isconst() { return true; }
	public void dop(qtree n, quaternion a, quaternion b, 
			double x, double y)
	{
	    n._v.qc1();
	}
    }
    
    public static class qop_qc2 extends qop {
	public String pname() { return "i"; }
	public int degree() { return 0; }
	public boolean isconst() { return true; }
	public void dop(qtree n, quaternion a, quaternion b, 
			double x, double y)
	{
	    n._v.qc2();
	}
    }
    
    public static class qop_qc3 extends qop {
	public String pname() { return "j"; }
	public int degree() { return 0; }
	public boolean isconst() { return true; }
	public void dop(qtree n, quaternion a, quaternion b, 
			double x, double y)
	{
	    n._v.qc3();
	}
    }
    
    public static class qop_qc4 extends qop {
	public String pname() { return "k"; }
	public int degree() { return 0; }
	public boolean isconst() { return true; }
	public void dop(qtree n, quaternion a, quaternion b, 
			double x, double y)
	{
	    n._v.qc4();
	}
    }
    
    public static class qop_qc5 extends qop {
	public String pname() { return "golden"; }
	public int degree() { return 0; }
	public boolean isconst() { return true; }
	public void dop(qtree n, quaternion a, quaternion b, 
			double x, double y)
	{
	    n._v.qc5();
	}
    }
    
    public static class qop_qcx extends qop {
	public String pname() { return "x"; }
	public int degree() { return 0; }
	public void dop(qtree n, quaternion a, quaternion b, 
			double x, double y)
	{
	    n._v.qcx(x,y);
	}
    }
    
    public static class qop_qcy extends qop {
	public String pname() { return "y"; }
	public int degree() { return 0; }
	public void dop(qtree n, quaternion a, quaternion b, 
			double x, double y)
	{
	    n._v.qcy(x,y);
	}
    }
    
    public static class qop_qcx1 extends qop {
	public String pname() { return "x_k"; }
	public int degree() { return 0; }
	public void dop(qtree n, quaternion a, quaternion b, 
			double x, double y)
	{
	    n._v.qcx1(x,y);
	}
    }
    
    public static class qop_qcy1 extends qop {
	public String pname() { return "y_k"; }
	public int degree() { return 0; }
	public void dop(qtree n, quaternion a, quaternion b, 
			double x, double y)
	{
	    n._v.qcy1(x,y);
	}
    }
    
    public static class qop_qcxy extends qop {
	public String pname() { return "x_iy"; }
	public int degree() { return 0; }
	public void dop(qtree n, quaternion a, quaternion b, 
			double x, double y)
	{
	    n._v.qcxy(x,y);
	}
    }
    
    public static class qop_qcxy2 extends qop {
	public String pname() { return "x_iy_jx_ky"; }
	public int degree() { return 0; }
	public void dop(qtree n, quaternion a, quaternion b, 
			double x, double y)
	{
	    n._v.qcxy2(x,y);
	}
    }
    
    public static class qop_qisin extends qop {
	public String pname() { return "isin"; }
	public int degree() { return 1; }
	public void dop(qtree n, quaternion a, quaternion b, 
			double x, double y)
	{
	    n._v.qisin(a);
	}
    }
    
    public static class qop_qilog extends qop {
	public String pname() { return "ilog"; }
	public int degree() { return 1; }
	public void dop(qtree n, quaternion a, quaternion b, 
			double x, double y)
	{
	    n._v.qilog(a);
	}
    }
    
    public static class qop_qiexp extends qop {
	public String pname() { return "iexp"; }
	public int degree() { return 1; }
	public void dop(qtree n, quaternion a, quaternion b, 
			double x, double y)
	{
	    n._v.qiexp(a);
	}
    }
    
    public static class qop_qimin extends qop {
	public String pname() { return "imin"; }
	public int degree() { return 2; }
	public void dop(qtree n, quaternion a, quaternion b, 
			double x, double y)
	{
	    n._v.qimin(a,b);
	}
    }
    
    public static class qop_qimax extends qop {
	public String pname() { return "imax"; }
	public int degree() { return 2; }
	public void dop(qtree n, quaternion a, quaternion b, 
			double x, double y)
	{
	    n._v.qimax(a,b);
	}
    }
    
    public static class qop_qrl extends qop {
	public String pname() { return "rolL"; }
	public int degree() { return 1; }
	public void dop(qtree n, quaternion a, quaternion b, 
			double x, double y)
	{
	    n._v.qrl(a);
	}
    }
    
    public static class qop_qrr extends qop {
	public String pname() { return "rolR"; }
	public int degree() { return 1; }
	public void dop(qtree n, quaternion a, quaternion b, 
			double x, double y)
	{
	    n._v.qrr(a);
	}
    }
    
    // control table:
    public final static qop[] qops = {
	new qop_qplus(),
	new qop_qsub(), new qop_qmult(), new qop_qinv(), new qop_qdiv(),
	new qop_qconj(), new qop_qaut1(), new qop_qaut2(), new qop_qexp(),
	new qop_qfloor(), new qop_qmod(), new qop_qnorm(), new qop_qnormp(),
	new qop_qorth1(), new qop_qorth2(), new qop_qc1(), new qop_qc2(),
	new qop_qc3(), new qop_qc4(), new qop_qc5(), new qop_qcx(),
	new qop_qcy(), new qop_qcx1(), new qop_qcy1(), new qop_qcxy(),
	new qop_qcxy2(), new qop_qisin(), new qop_qilog(), new qop_qiexp(),
	new qop_qimin(), new qop_qimax(), new qop_qrl(), new qop_qrr()};
    public final static qop default_op = new qop_qcxy();

    // tree-node variables
    private qop _op = null;
    private qtree _l = null;
    private qtree _r = null;
    private qtree _parent = null;
    private transient boolean _isconsttree = false;
    private transient quaternion _vl = null;
    private transient quaternion _vr = null;
    private transient quaternion _v = null;
    
    public Object clone()
    {
	return rclone(null);
    }

    // copy non-transient fields
    qtree rclone(qtree p)
    {
	qtree r = null;
	if(this==null) {
	    return null;
	}
	r = new qtree();
	r._parent = p;
	r._op = _op;
	if(_l!=null) {
	    r._l = _l.rclone(r);
	}
	if(_r!=null) {
	    r._r = _r.rclone(r);
	}
	return r;
    }

    qtree()
    {
	_v = new quaternion();
    }
    
    // compile the tree to go (sets constants etc)
    public qtree[] compile()
    {
	pre_eval_tree();
	int k = nonconstnodes(0);
	qtree[] vec = null;
	if(k>0) {
	    vec = new qtree[k];
	    pre_dispatch_tree(vec,0);
	}
	return vec;
    }

    // run through tree, evaluate at 0,0 to set constants
    // and mark constant sub-trees, also set left/right 
    // quaterinon links
    void pre_eval_tree()
    {
	qop q = _op;
	_vl = null;
	_vr = null;
	_isconsttree = true;
	if(q==null) {
	    return;
	}
	int d = q.degree();
	if(d>0) {
	    _l.pre_eval_tree();
	    _vl = _l._v;
	    if(!_l._isconsttree) {
		_isconsttree = false;
	    }
	    if(d>1) {
		_r.pre_eval_tree();
		_vr = _r._v;
		if(!_r._isconsttree) {
		    _isconsttree = false;
		}
	    }
	} else {
	    if(!q.isconst()) {
		_isconsttree = false;
	    }
	}
	// evaluate the whole tree at 0,0 as a side-effect
	_op.dop(this,_vl,_vr,0.0,0.0);
    }

    // count non-const nodes, depends on pre_eval_tree
    int nonconstnodes(int start)
    {
	qop q = _op;
	if((q==null)||(_isconsttree)) {
	    return start;
	}
	int d = q.degree();
	if(d>0) {
	    start = _l.nonconstnodes(start);
	}
	start += 1; //self
	if(d>1) {
	    start = _r.nonconstnodes(start);
	}
	return start;
    }

    // flatten recursive traversal of tree- pre-order traversal
    // as a side-effect set constants in tree
    // these constants are not re-evaluated, could eliminate
    //  common sub-expressions also, but I don't know if we
    //  have a lot of these.
    int pre_dispatch_tree(qtree[] v, int start)
    {
	qop q = _op;
	if((q==null)||(_isconsttree)) {
	    return start;
	}
	int d = q.degree();
	if(d>0) {
	    if(!_l._isconsttree) {
		start = _l.pre_dispatch_tree(v,start);
	    }
	    if(d>1) {
		if(!_r._isconsttree) {
		    start = _r.pre_dispatch_tree(v,start);
		}
	    }
	}
	if(!_isconsttree) {
	    v[start] = this;
	    start += 1; // self
	}
	return start;
    }
    
    // use the flattened traversal to evaluate tree
    void dispatch_tree(double x, double y, qtree[] v)
    {
	for(int i=0;i<v.length;++i) {
	    qtree t;
	    t = v[i];
	    t._op.dop(t,t._vl,t._vr,x,y);
	}
    }
    
    public static int crunch(double x)
    {
	if((x>30.0)||(x<-30.0)) {
	    x = 0.0;
	} else {
	    x = 1.0/(1.0+Math.exp(-1.0*x));
	}
	return (int)Math.floor(255.0*x);
    }
    
    public Color evalTree(double x, double y, qtree[] v)
    {
	if(v!=null) {
	    dispatch_tree(x,y,v);
	    quaternion q = _v;
	    int r = crunch(q.getI());
	    int g = crunch(q.getJ());
	    int b = crunch(q.getK());
	    // System.out.println("(" + x + "," + y + ") R=" + r + " G=" + g + " B= " + b);
	    return new Color(r,g,b);
	}
	// else
	return new Color(127,127,127);
    }
    
    // insert a null tok if not found, return degree
    // convert + to _ in tokens of length > 1
    void findtok(String si)
    {
	int f;
	String s;
	
	if(si.length()>1) {
	    s = si.replace('+','_');
	} else {
	    s = si;
	}
	
	f = -1;
	for(int i=0;(f<0)&&(i<qops.length);++i) {
	    if(s.equals(qops[i].pname())) {
		f = i;
	    }
	}
	if(f>=0) {
	    // System.out.println("find \"" + s + "\" found \"" + qops[f].pname() + "\"");
	    _op = qops[f];
	} else {
	    // System.out.println("find \"" + s + "\" missed");
	    _op = default_op;
	}
    }
    
    // attach first token to this node and recurse
    int parse(String s, int offset)
    {
	int a,b;
	
	// some safe defaults
	_l = null;
	_r = null;
	_isconsttree = false;
	_op = default_op;
	if((s==null)||(s.length()<1)) {
	    return offset;
	}
	a = offset;
	// skip whitespace
	while(a<s.length()) {
	    char ch = s.charAt(a);
	    if((Character.isWhitespace(ch))||(ch=='(')||(ch==')')) {
		++a;
	    } else {
		break;
	    }
	}
	b = a;
	if(a<s.length()) {
	    b = a+1;
	    while(b<s.length()) {
		char ch = s.charAt(b);
		if((Character.isWhitespace(ch))||(ch=='(')||(ch==')')) {
		    break;
		} else {
		    ++b;
		}
	    }
	    String t = s.substring(a,b);
	    findtok(t);
	    if((_op!=null)&&(_op.degree()>0)) {
		_l = new qtree();
		_l._parent = this;
		b = _l.parse(s,b);
		if(_op.degree()>1) {
		    _r = new qtree();
		    _r._parent = this;
		    b = _r.parse(s,b);
		}
	    }
	}
	return b;
    }
    
    // print tree, prints to stdout
    public void printtree()
    {
	String s = toString();
	System.out.println(s);
    }

    // tree to string, needs final println() after done
    public String toString()
    {
	qop q = null;
	if(this!=null) {
	    q = _op;
	}
	if(q==null) {
	    if(this==null) {
		return "NULL";
	    } else {
		return "null";
	    }
	} else {
	    if(q.degree()==0) {
		return q.pname();
	    } else if(q.degree()==1) {
		String nl;
		if(_l==null) {
		    nl = "NULL";
		} else {
		    nl = _l.toString();
		}
		return "( " + q.pname() + " " + nl + " )";
	    } else {
		String nl;
		if(_l==null) {
		    nl = "NULL";
		} else {
		    nl = _l.toString();
		}
		String nr;
		if(_r==null) {
		    nr = "NULL";
		} else {
		    nr = _r.toString();
		}
		return "( " + q.pname() + " " + nl + " " + nr + " )";
	    }
	}
    }
    
    qtree(String s)
    {
	_v = new quaternion();
	parse(s,0);
	// printtree();
    }

    // side effect variable nodes() is allowed to set
    static qtree lastmatch = null;

    // count non-null nodes in tree, inorder traversal made obvious
    // also can pick up a matching node (count starts at 0)
    // allowed to alter lastmatch variable
    // call with nodes(0,target)
    public int nodes(int start, int target)
    {
	qop q = _op;
	if(q==null) {
	    return start;
	}
	int d = q.degree();
	if(d>0) {
	    start = _l.nodes(start,target);
	}
	if(start==target) {
	    // found matching node
	    lastmatch = this;
	}
	start += 1; //self
	if(d>1) {
	    start = _r.nodes(start,target);
	}
	return start;
    }

    // get a random op of given degree, -1 means any degree
    // fix: make a lookup
    static qop randop(int d) 
    {
	if(d<0) {
	    int n = qops.length;
	    int k = (int)(Math.random()*n);
	    return qops[k];
	} else {
	    int n = 0;
	    for(int i=0;i<qops.length;++i) {
		if(qops[i].degree()==d) {
		    ++n;
		}
	    }
	    int k = (int)(Math.random()*n);
	    for(int i=0;(k>=0)&&(i<qops.length);++i) {
		if(qops[i].degree()==d) {
		    if(k==0) {
			return qops[i];
		    }
		    --k;
		}
	    }
	}
	return null; 
    }

    // create a new tree
    public qtree breed(qtree t)
    {
	qtree r = null;
	qtree t2 = null;
	
	// maybe whole new tree
	if(Math.random()<.3) {
	    return new qtree(8);
	}
	///System.out.println("// formula source = crossover");

	if(((int)(Math.random()*2))==0) {
	    r = (qtree)clone();
	    t2 = t;
	} else {
	    r = (qtree)t.clone();
	    t2 = this;
	}

	int s1 = r.nodes(0,-1);
	int r1 = (int)(Math.random()*s1);
	r.nodes(0,r1);
	qtree q1 = r.lastmatch;

	int s2 = t2.nodes(0,-1);
	int r2 = (int)(Math.random()*s2);
	t2.nodes(0,r2);
	qtree q2 = t2.lastmatch;

	// do the graft
	boolean isleft = true;
	qtree par = q1._parent;
	qtree st = (qtree)q2.clone();
	if(par!=null) {
	    if(par._l==q1) {
		par._l = null;
		isleft = true;
	    } else {
		par._r = null;
		isleft = false;
	    }
	}
	// System.out.println();
	// System.out.println("host");
	// r.printtree();
	// System.out.println("donation");
	// st.printtree();
	if(par!=null) {
	    if(isleft) {
		par._l = st;
	    } else {
		par._r = st;
	    }
	}
	st._parent = par;
	// System.out.println("result");
	// r.printtree();

	// mutation
	s1 = r.nodes(0,-1);
	r1 = (int)(Math.random()*s1);
	r.nodes(0,r1);
	q1 = r.lastmatch;
	q1._op = randop(q1._op.degree());

	return r;
    }

    // breed a tree
    public static qtree sbreed(String s1, String s2)
    {
	qtree t1 = new qtree(s1);
	qtree t2 = new qtree(s2);
	qtree t3 = t1.breed(t2);
	return t3;
    }

    // random tree- nast probability model (not much care)
    // or go to arcive
    qtree(int l)
    {
	_v = new quaternion();
	if(Math.random()<0.5) {
	    //System.out.println("// formula source = random");
	    r_rantree(l,null);
	} else {
	    //System.out.println("// formula source = archive");
	    int n = farchive.flist.length;
	    int k = (int)(n*Math.random());
	    parse(farchive.flist[k],0);
	}
    }

    void r_rantree(int l, qtree p)
    {
	_parent = p;
	if(l<=0) {
	    // terminate tree
	    _op = randop(0);
	} else {
	    // pick degree of node, prefer degree 2
	    int d = (int)(Math.random()*5);
	    if(d>2) {
		d = 2;
	    }
	    _op = randop(d);
	}
	if(_op.degree()>0) {
	    _l = new qtree();
	    _l.r_rantree(l-1,this);
	    if(_op.degree()>1) {
		_r = new qtree();
		_r.r_rantree(l-1,this);
	    }
	}
    }

    public void picfromform(int pwidth, int pheight, 
			    Image img)
    {
	Graphics g = img.getGraphics();
	if(this==null) {
	    Color c2 = new Color(127,0,0);
	    g.setColor(c2);
	    g.fillRect(0,0,pwidth,pheight);
	} else {
	    int span;
	    
	    if(pwidth>=pheight) {
		span = pheight;
	    } else {
		span = pwidth;
	    }
	    // compile the tree
	    qtree[] vec = compile();
	    // evaluate 
	    for(int i=0;i<pwidth;++i) {
		double x = (i - pwidth/2.0)/((double)span);
		for(int j=0;j<pheight;++j) {
		    double y = (j - pheight/2.0)/((double)span);
		    Color c = evalTree(x,y,vec);
		    // fix: use some other method than rect
		    g.setColor(c);
		    g.fillRect(i,j,1,1);
		}
	    }
	}
    }
    
}

