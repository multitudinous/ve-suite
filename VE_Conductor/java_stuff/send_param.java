import java.io.*;
import java.lang.String;
import java.util.ArrayList;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.text.*;

public class send_param extends JPanel
{
 ArrayList my_params = new ArrayList();
 private WholeNumberField[] e_Field;
 private int[] param_value;
 MyDocumentListener myDocumentListener = new MyDocumentListener();
 JPanel button_panel = new JPanel();

 public send_param()
 {
  String tmp_param = new String();
  setLayout(new BoxLayout(this,BoxLayout.Y_AXIS));

  try
  {
   FileInputStream paramFile = new FileInputStream("./config/param.config");
   BufferedReader r = new BufferedReader(new InputStreamReader(paramFile));

   while((tmp_param = r.readLine()) != null)
   {
    my_params.add((Object)tmp_param);
   }

   r.close();
  }
  catch(java.io.IOException e)
  {
   System.out.println("Read file error"+e);
  }

  JLabel[] param_names;
  param_names = new JLabel[my_params.size()];
  param_value = new int[my_params.size()];
  e_Field = new WholeNumberField[my_params.size()];
  JPanel label_panel = new JPanel();
  label_panel.setLayout(new GridLayout(my_params.size(),2));
  
  for ( int i = 0; i < my_params.size(); i++)
  {
    param_names[i] = new JLabel("<html>\n"+"<font size=2>"+(String)my_params.get(i)+"</font>\n");
    e_Field[i] = new WholeNumberField(0,2);
    e_Field[i].getDocument().addDocumentListener(myDocumentListener);
    e_Field[i].getDocument().putProperty("name", "my_param"+String.valueOf(i));
    param_names[i].setLabelFor(e_Field[i]);
    label_panel.add(param_names[i]);
    label_panel.add(e_Field[i]);
  }

  add(label_panel);
  add(button_panel);
 }

 public void add_but(JButton b)
 {
  button_panel.add(b);
 }

 public int[] get_param()
 {
  return param_value;
 }

  private class MyDocumentListener implements DocumentListener {
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

	    for ( int i = 0; i < my_params.size(); i++)
	     {
              if (whatsup.getProperty("name").equals("my_param"+String.valueOf(i)))
                param_value[i]= e_Field[i].getValue();
	     }

    }
 } 
}		
  