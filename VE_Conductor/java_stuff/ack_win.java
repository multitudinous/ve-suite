
import javax.swing.*;
import javax.swing.event.*;
import java.awt.*;
import java.awt.event.*;

public class ack_win extends Thread
{
 private int type;
 private String dest;
 private JFrame frame;

 public ack_win(String d, int t, JFrame f)
 {
  dest = d;
  type = t;
  frame = f;
 }

 public void run()
 {
  if(type ==1)
   JOptionPane.showMessageDialog(frame, "The job on "+dest+" finished!",
		 "message from server", JOptionPane.PLAIN_MESSAGE);
  if(type ==2)
   JOptionPane.showMessageDialog(frame, "The destination "+dest,
		 "message from server", JOptionPane.PLAIN_MESSAGE);
 }
}