

// $Header: C:\\JMount\\geneticArt/RCS/gpic.java,v 1.2 1999/05/30 01:42:03 jmount Exp $

import java.awt.*;
import java.awt.event.*;
import java.applet.*;

// using older graphics methods so we will run on browsers we
// are likely to encounter.

import java.util.*;

public class gpic extends Applet {
    private int pwidth = 80;   // size of graphic 
    private int pheight = 60;  // size of graphic 
    private Image image = null;       // backing store
    private qtree formula = null;     // formula

    public synchronized void init() {
	formula = new qtree(7);
	image = null;
    }

    public synchronized void paint(Graphics g) {
	if(image!=null) {
	    g.drawImage(image,0,0,this);
	}
    }

    synchronized void updateimg() {
	if(formula!=null) {
	    if(image==null) {
		image = createImage(pwidth,pheight);
	    }
	    formula.picfromform(pwidth,pheight,image);
	    repaint();
	}
    }

    public synchronized void setwidthheight(int w, int h) {
	image = null;
	pwidth = w;
	pheight = h;
	updateimg();
    }

    public synchronized void setformula(String s) {
	formula = new qtree(s);
	updateimg();
    }

    public synchronized void randformula(int k) {
	formula = new qtree(7);
	updateimg();
    }

    public synchronized void offspring(String s1, String s2) {
	formula = qtree.sbreed(s1,s2);
	updateimg();
    }

    public synchronized String getFormula() {
	if(formula==null) {
	    return "NULL";
	} else {
	    return formula.toString();
	}
    }

    public String getAppletInfo() {
	return "Genetic Art3 by John Mount";
    }
}
