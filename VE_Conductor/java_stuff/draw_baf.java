import java.awt.*;
import java.awt.geom.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.text.*;
import java.text.*;

public class draw_baf extends JPanel {

    int counter;
    static String[] baf_info;
    private JLabel[] e_label;
    private WholeNumberField[] e_Field;
    public short[] param;
    String convert = new String();

    private short canvas_size = 380;
    
    class baf
    {
	 public int aa;
	 public int bb;
	 public int cc;
	 public int dd;
	 public int ee;

	public baf()
	{
	 aa = 0;
	 bb = 0;
	 cc = 0;
	 dd = 0;
	 ee = 0;
	}
    }

    public baf[] my_baf;
     
    class draw_canv extends Canvas
    {
     public Point2D.Float[] my_points;

     public draw_canv()
     {
      my_points = new Point2D.Float[6];
      HitTestMouseListener listener = new HitTestMouseListener();
      addMouseListener(listener);
      addMouseMotionListener(listener);
      setVisible(false);
     }
     
     public Point2D.Float[] get_point()
     {
      return my_points;
     } 

     public void paint(Graphics g)
     {
	Color fg3D ;

        if(counter == 1 || counter == 3 || counter >= 5)
	{
	 Graphics2D g2 = (Graphics2D) g;
         g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        
         for( int i = 0; i < counter; i=i+2)
	 {
	  if(i == 0)
	   fg3D = Color.red;
	  else if(i == 2)
	   fg3D = Color.blue;
	  else
	   fg3D = Color.green;

	  g2.setPaint(fg3D);

	  if (Math.abs(my_points[i+1].getX()-my_points[i].getX())<Math.abs(my_points[i+1].getY()-my_points[i].getY()))
	  {
	   g2.draw(new Line2D.Double(my_points[i].getX(), my_points[i].getY(),
			    my_points[i].getX(), my_points[i+1].getY()));  //y-direction

	   if (my_points[i].getY()>my_points[i+1].getY())
	   {
	    my_baf[i/2].aa = (int)(42*(280 - (int)my_points[i].getY())/canvas_size);
	    my_baf[i/2].cc = 1;  //+y
	    my_baf[i/2].dd = (int)(42*(my_points[i].getY() - my_points[i+1].getY())/canvas_size);
	   }
	   else
	   {
	    my_baf[i/2].aa = (int)(42*(280 - (int)my_points[i+1].getY())/canvas_size);
	    my_baf[i/2].cc = 3;  //-y
	    my_baf[i/2].dd = (int)(42*(my_points[i+1].getY() - my_points[i].getY())/canvas_size);
	   }

	   my_baf[i/2].bb = (int)(42*my_points[i].getX()/canvas_size);
	   my_points[i+1].setLocation(my_points[i].getX(),my_points[i+1].getY());
	  }
	  else
	  {
	   g2.draw(new Line2D.Double(my_points[i].getX(), my_points[i].getY(),
			    my_points[i+1].getX(), my_points[i].getY()));  //x-direction

	   if (my_points[i].getX()<my_points[i+1].getX())
	   {
	    my_baf[i/2].aa = (int)(42*my_points[i].getX()/canvas_size);
	    my_baf[i/2].cc = 0;  //+x
	    my_baf[i/2].dd = (int)(42*(my_points[i+1].getX() - my_points[i].getX())/canvas_size);
	   }
	   else
	   {
	    my_baf[i/2].aa = (int)(42*my_points[i+1].getX()/canvas_size);
	    my_baf[i/2].cc = 2;  //-x
	    my_baf[i/2].dd = (int)(42*(my_points[i].getX() - my_points[i+1].getX())/canvas_size);
	   }

	   my_baf[i/2].bb = (int)(42*(280 - (int)my_points[i].getY())/canvas_size);
	   my_points[i+1].setLocation(my_points[i+1].getX(),my_points[i].getY());
	  }
	 }
   	}
     }

	private class HitTestMouseListener extends MouseInputAdapter {

	Point2D.Float tmp_point = new Point2D.Float();
	private double oldv;
	private double dist;
	public int id;

        public void mouseClicked(MouseEvent e) {

           if(counter <6)
	   {
	     my_points[counter] = new Point2D.Float(e.getX(),e.getY());
            
	    counter++;

            }
        }

	public void mouseMoved(MouseEvent e) {

	if(counter == 1 || counter == 3 || counter == 5)
	{            
	   my_points[counter] = new Point2D.Float(e.getX(),e.getY());
	   repaint();
	}
        }

	public void mousePressed(MouseEvent e) {

	oldv = 1000.0;
	if(counter > 6)
	{
	 tmp_point.setLocation(e.getX(),e.getY());

	 for( int i = 0; i < 6; i++)
	 {
	  dist = tmp_point.distance(my_points[i].getX(),my_points[i].getY());
	  if(dist<oldv)
	  {
	   oldv = dist;
	   id = i;
	  }
	 }
	}
	}	 

	public void mouseDragged(MouseEvent e) {

	 if(dist<10.0)
	 {
	 //draw a circle
	  my_points[id].setLocation(e.getX(),e.getY());
	  repaint();
	 }
	}
	
  }
 }

     JPanel control_panel = new JPanel();
     JPanel button_panel = new JPanel();
     JPanel sender_panel = new JPanel();
     JPanel choice_panel = new JPanel();
     JPanel sender_button_panel = new JPanel();
     JPanel text_panel = new JPanel();
     

     draw_canv canv = new draw_canv();
     static JLabel[] baf_param;
     boolean first_time;
     JPanel labelPane = new JPanel();
     JPanel fieldPane = new JPanel();
     JPanel tPane = new JPanel();

    public draw_baf() 
    {
        counter = 0;
	baf_info = new String[3];
	baf_param = new JLabel[3];
	my_baf = new baf[3];
	e_label = new JLabel[3];
	e_Field = new WholeNumberField[3];
	MyDocumentListener myDocumentListener = new MyDocumentListener();
	String convert = new String();
	param = new short[15];
	labelPane.setLayout(new GridLayout(0, 1));
	fieldPane.setLayout(new GridLayout(0, 1));

	e_label[0] = new JLabel("<html>\n"+"<font color=red size=-2>E:</font>");
	e_label[1] = new JLabel("<html>\n"+"<font color=blue size=-2>E:</font>");
	e_label[2] = new JLabel("<html>\n"+"<font color=green size=-2>E:</font>");

	for (int i = 0; i < 3; i++)
	{
	 my_baf[i] = new baf();
	 e_Field[i] = new WholeNumberField(0,2);
	 e_Field[i].getDocument().addDocumentListener(myDocumentListener);
	 e_Field[i].getDocument().putProperty("name", "my_baf"+convert.valueOf(i));
	 	
	 e_label[i].setLabelFor(e_Field[i]);
	} 

	control_panel.setPreferredSize(new Dimension(620-canvas_size, 280));
	control_panel.setMinimumSize(new Dimension(620-canvas_size, 280));
	control_panel.setLayout(new BoxLayout(control_panel,BoxLayout.Y_AXIS));

	button_panel.setPreferredSize(new Dimension(620-canvas_size, 45));
	button_panel.setMinimumSize(new Dimension(620-canvas_size, 45));
	//button_panel.setLayout(new BoxLayout(button_panel,BoxLayout.X_AXIS));

	sender_panel.setPreferredSize(new Dimension(620-canvas_size, 60));
	sender_panel.setMinimumSize(new Dimension(620-canvas_size, 60));
	//sender_panel.setLayout(new BoxLayout(sender_panel,BoxLayout.X_AXIS));

	choice_panel.setLayout(new BoxLayout(choice_panel,BoxLayout.Y_AXIS));
	choice_panel.setPreferredSize(new Dimension((620-canvas_size)/2-10,50));
	choice_panel.setMinimumSize(new Dimension((620-canvas_size)/2-10,50));

	JScrollPane cscroll = new JScrollPane(choice_panel);
	cscroll.setPreferredSize(new Dimension((620-canvas_size)/2, 60));
	cscroll.setMinimumSize(new Dimension((620-canvas_size)/2, 60));

	text_panel.setLayout(new BoxLayout(text_panel,BoxLayout.Y_AXIS));
	text_panel.setPreferredSize(new Dimension(620-canvas_size,170));
	text_panel.setMinimumSize(new Dimension(620-canvas_size, 170));

	set_text();

	sender_panel.add(cscroll);
	sender_panel.add(sender_button_panel);
	control_panel.add(button_panel);
	control_panel.add(sender_panel);
	control_panel.add(text_panel);

	JScrollPane ascroll = new JScrollPane(control_panel);
	ascroll.setPreferredSize(new Dimension(630-canvas_size, canvas_size));
	ascroll.setMinimumSize(new Dimension(630-canvas_size, canvas_size));

	canv.setSize(new Dimension(canvas_size, canvas_size));

	first_time = true;

	add(canv);
	add(ascroll);

	}

    private void set_text()
    {
       baf_info[0] = "<html>\n"+"<font color=red size=-1>A:</font>\n"
		      +"<font color=red size=-2>"+convert.valueOf(my_baf[0].aa)+"</font>\n"
		      +"<font color=red size=-2>B:</font>\n"
		      +"<font color=red size=-2>"+convert.valueOf(my_baf[0].bb)+"</font>\n"
		      +"<font color=red size=-2>C:</font>\n"
		      +"<font color=red size=-2>"+convert.valueOf(my_baf[0].cc)+"</font>\n"
		      +"<font color=red size=-2>D:</font>\n"
		      +"<font color=red size=-2>"+convert.valueOf(my_baf[0].dd)+"</font>\n";

	baf_info[1] = "<html>\n"+"<font color=blue size=-1>A:</font>\n"
		      +"<font color=blue size=-2>"+convert.valueOf(my_baf[1].aa)+"</font>\n"
		      +"<font color=blue size=-2>B:</font>\n"
		      +"<font color=blue size=-2>"+convert.valueOf(my_baf[1].bb)+"</font>\n"
		      +"<font color=blue size=-2>C:</font>\n"
		      +"<font color=blue size=-2>"+convert.valueOf(my_baf[1].cc)+"</font>\n"
		      +"<font color=blue size=-2>D:</font>\n"
		      +"<font color=blue size=-2>"+convert.valueOf(my_baf[1].dd)+"</font>\n";
		      
	baf_info[2] = "<html>\n"+"<font color=green size=-1>A:</font>\n"
		      +"<font color=green size=-2>"+convert.valueOf(my_baf[2].aa)+"</font>\n"
		      +"<font color=green size=-2>B:</font>\n"
		      +"<font color=green size=-2>"+convert.valueOf(my_baf[2].bb)+"</font>\n"
		      +"<font color=green size=-2>C:</font>\n"
		      +"<font color=green size=-2>"+convert.valueOf(my_baf[2].cc)+"</font>\n"
		      +"<font color=green size=-2>D:</font>\n"
		      +"<font color=green size=-2>"+convert.valueOf(my_baf[2].dd)+"</font>\n";

	
	 baf_param[0] = new JLabel(baf_info[0]);
	 baf_param[1] = new JLabel(baf_info[1]);
 	 baf_param[2] = new JLabel(baf_info[2]);
	 text_panel.add(baf_param[0]);
	 text_panel.add(baf_param[1]);
	 text_panel.add(baf_param[2]);

	 for (int i = 0; i < 3; i++)
	 {
	  labelPane.add(e_label[i]);
	  fieldPane.add(e_Field[i]);
	 }

	 tPane.add(labelPane);
	 tPane.add(fieldPane);
	 text_panel.add(tPane);

	 text_panel.repaint();
	 	
    }

    public void update_text()
    {
	baf_info[0] = "<html>\n"+"<font color=red size=-1>A:</font>\n"
		      +"<font color=red size=-2>"+convert.valueOf(my_baf[0].aa)+"</font>\n"
		      +"<font color=red size=-2>B:</font>\n"
		      +"<font color=red size=-2>"+convert.valueOf(my_baf[0].bb)+"</font>\n"
		      +"<font color=red size=-2>C:</font>\n"
		      +"<font color=red size=-2>"+convert.valueOf(my_baf[0].cc)+"</font>\n"
		      +"<font color=red size=-2>D:</font>\n"
		      +"<font color=red size=-2>"+convert.valueOf(my_baf[0].dd)+"</font>\n";

	baf_info[1] = "<html>\n"+"<font color=blue size=-1>A:</font>\n"
		      +"<font color=blue size=-2>"+convert.valueOf(my_baf[1].aa)+"</font>\n"
		      +"<font color=blue size=-2>B:</font>\n"
		      +"<font color=blue size=-2>"+convert.valueOf(my_baf[1].bb)+"</font>\n"
		      +"<font color=blue size=-2>C:</font>\n"
		      +"<font color=blue size=-2>"+convert.valueOf(my_baf[1].cc)+"</font>\n"
		      +"<font color=blue size=-2>D:</font>\n"
		      +"<font color=blue size=-2>"+convert.valueOf(my_baf[1].dd)+"</font>\n";
		      
	baf_info[2] = "<html>\n"+"<font color=green size=-1>A:</font>\n"
		      +"<font color=green size=-2>"+convert.valueOf(my_baf[2].aa)+"</font>\n"
		      +"<font color=green size=-2>B:</font>\n"
		      +"<font color=green size=-2>"+convert.valueOf(my_baf[2].bb)+"</font>\n"
		      +"<font color=green size=-2>C:</font>\n"
		      +"<font color=green size=-2>"+convert.valueOf(my_baf[2].cc)+"</font>\n"
		      +"<font color=green size=-2>D:</font>\n"
		      +"<font color=green size=-2>"+convert.valueOf(my_baf[2].dd)+"</font>\n";

     	 baf_param[0].setText(baf_info[0]);
	 baf_param[1].setText(baf_info[1]);
	 baf_param[2].setText(baf_info[2]);
	
	repaint();

    }

    public void clear()
     {
      counter = 0;
      canv.repaint();
     }

    public void convert_p()
    {
     int ii = 0;

     for( int i = 0; i < 3; i++)
     {
	param[ii] = (short)my_baf[i].aa;
	param[ii+1] = (short)my_baf[i].bb;
	param[ii+2] = (short)my_baf[i].cc;
	param[ii+3] = (short)my_baf[i].dd;
	param[ii+4] = (short)my_baf[i].ee;
	
	ii = ii+5;
     }
    }

    class MyDocumentListener implements DocumentListener {
        public void insertUpdate(DocumentEvent e) {
            calculateValue(e);
        }
        public void removeUpdate(DocumentEvent e) {
            calculateValue(e);
        }
        public void changedUpdate(DocumentEvent e) {
            // we won't ever get this with PlainDocument
        }
        private void calculateValue(DocumentEvent e) {
            Document whatsup = e.getDocument();
            if (whatsup.getProperty("name").equals("my_baf0"))
                my_baf[0].ee = e_Field[0].getValue();
            else if (whatsup.getProperty("name").equals("my_baf1"))
                my_baf[1].ee = e_Field[1].getValue();
            else if (whatsup.getProperty("name").equals("my_baf2"))
                my_baf[2].ee = e_Field[2].getValue();
           }
    }

}
