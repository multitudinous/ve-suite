import java.io.*;
import java.lang.String;
import java.util.ArrayList;

import java.awt.*;
import java.awt.event.*;

import java.lang.reflect.*;
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.text.*;

import java.text.*;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

public class send_operation extends JPanel
                               implements PropertyChangeListener

{
 ArrayList my_design_params = new ArrayList();
 NumberFormat e_Formatter ;
 private general_Field[] e_Field;
 private double [] design_param_values;
 //MyDocumentListener myDocumentListener = new MyDocumentListener();

//@@@@@@0. define graphical panel with name 'button_panel'
 JPanel button_panel = new JPanel();

 public send_operation()
 {
  String tmp_param = new String();
  setLayout(new BoxLayout(this,BoxLayout.Y_AXIS));

  try
  {
//@@@@@@1. read paramters's name and values from param.config into BufferedReader

   FileInputStream paramFile = new FileInputStream("./config/param.config");
   BufferedReader r = new BufferedReader(new InputStreamReader(paramFile));

//@@@@@@2. read each line and store each streamline into object named 'my_design_params'
   while((tmp_param = r.readLine()) != null)
   {
    my_design_params.add((Object)tmp_param);
   }

   r.close();
  }
  catch(java.io.IOException e)
  {
   System.out.println("Read file error"+e);
  }

  JLabel[] design_param_names;
  //Create the lables
//@@@@@@3. Instantiate array of 'design_param_names' and 'design_param_halues' with size of 'my_design_params'
  design_param_names = new JLabel[my_design_params.size()];
  design_param_values = new double[my_design_params.size()];

//@@@@@@4. Instantiate an array of text fields with size of my_design_params.size()

  //Create the text fields and set them up
  e_Field   = new general_Field[my_design_params.size()];
  //for (int i = 0; i < 8; ++i)
  //loop size as that of 'my_design_params'
  for (int i = 0; i < my_design_params.size(); ++i)
  {
      e_Field[i] = new general_Field();
  }
  JPanel label_panel = new JPanel();

//@@@@@@5. Define panal layout as a grid, row number is decided by 'my_design_params.size()', column number is '2'
  label_panel.setLayout(new GridLayout(my_design_params.size(),2));

  //initialize the panel
//@@@@@@6. Set text into JLabel field, such as "Agitation" stored in my_design_params.get(0)
  for ( int i = 0; i < my_design_params.size(); i++)
  {
    design_param_names[i] = new JLabel("<html>\n"+"<font size=2>"+(String)my_design_params.get(i)+"</font>\n");
  //  design_param_values[i]=2.0;
  }
//@@@@@@7. Store initial value into array of double named 'design_param_values'

  design_param_values[0]=200;	//Agitation
  design_param_values[1]=1.25; 	//Air Concentration
  design_param_values[2]=6;		//Initial pH value
  design_param_values[3]=0.1;	//Nitrate Concentration
  design_param_values[4]=37;	//Temperature (Celsius)
  design_param_values[5]=240;	//Simuate Hours in 10 Seconds

  System.out.println("[DBG] e_Field size: " + Array.getLength(e_Field));
  for ( int i =0; i < my_design_params.size(); i++)
  {
    System.out.println("[DBG] Accessing e_Field[" + i + "]");
    setUpFormats();
     if (e_Formatter == null)
     {
         System.out.println("[ERR] e_Formatter is null");
     }
     if (e_Field[i] == null)
     {
         System.out.println("[ERR] e_Field[i] is null");
     }
//@@@@@@8. Format and Set each text field with the value from 'design_param_values'

    e_Field[i].gField = new JFormattedTextField (e_Formatter );
    (e_Field[i].gField).setValue(new Double(design_param_values[i]));
    (e_Field[i].gField).setColumns(5);
    (e_Field[i].gField).addPropertyChangeListener("value",this);

    //lable/text field pairs
    design_param_names[i].setLabelFor(e_Field[i]);

//@@@@@@9. Add pairs of lable field and text field into panel
    label_panel.add(design_param_names[i]);
    label_panel.add(e_Field[i].gField);
  }

  add(label_panel);
  add(button_panel);
 }

/** Called when a field's "value" property changes. */
    public void propertyChange(PropertyChangeEvent e)
    {
         Object source = e.getSource();
         System.err.println("[DBG]....test listener" );
         for( int i =0; i < my_design_params.size();i++)
         {
            design_param_values[i]=((Number)(e_Field[i].gField).getValue()).doubleValue();
            System.err.println("[DBG]...." + design_param_values[i]);


         }


    }

 private void setUpFormats()
 {
      e_Formatter = NumberFormat.getNumberInstance();
      e_Formatter.setMinimumFractionDigits(3);
 }

 public void add_but(JButton b)
 {
  button_panel.add(b);
 }

 public double[] get_design_param()
 {
  return design_param_values;
 }

}

