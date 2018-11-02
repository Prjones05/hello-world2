using System;
using System.Web.Mail;
using System.Collections;
using System.Collections.Generic;
using System.Text;

namespace RateAvailableServiceWebServiceClient
{
    class EmailMgr
    {
        public static string ModName = "EmailMgr";
        public string Label = "FedEx Rate Check";
        public static string No_RMA_Msg = "** Error Notification **";
        public DateTime DateTime = DateTime.Now;
        public Boolean Run_Process = false;

        public EmailMgr()
        {
            //Empty Constructor
        }

        public static void mProcessMail(string mMessage)
        {
            try
            {
                DateTime DateTime = DateTime.Now;

                string mMailBody = " \r\n ";
                mMailBody = mMailBody + "Esker Processing Error:  " + DateTime.ToLongDateString() + " - " + DateTime.ToLongTimeString();
                mMailBody = mMailBody + " \r\n \r\n\r\n \r\n";

                mMailBody = mMailBody + "   ...    " + mMessage;

                mMailBody = mMailBody + " \r\n \r\n\r\n \r\n";
                mMailBody = mMailBody + " Esker";
                mMailBody = mMailBody + " \r\n \r\n\r\n \r\n";
                mMailBody = mMailBody + " \r\n \r\n\r\n \r\n";


                mMailBody = mMailBody + "\r\n\t\t\t                                                       ";
                mMailBody = mMailBody + "\r\n\t\t\t                    Esker RMA Processing \r\n \r\n\r\n ";
                mMailBody = mMailBody + "\r\n\t\t    © 2008 CarSound Exhaust Systems, Inc. All rights reserved.";

                MailMessage mail = new MailMessage();

                mail.To = "pjones@magnaflow.com";
                mail.From = "pjones@magnaflow.com"; // "pjones@magnaflow.com";
                mail.Subject = "FedEx Rate Check: " + DateTime.ToLongDateString() + " - " + DateTime.ToLongTimeString();
                mail.Body = mMailBody;

                SmtpMail.SmtpServer = "CSAVSMAIL1.magnaflow.com";
                SmtpMail.Send(mail);

                IO.mWrite_To_EventLog("Email Mgr Module: Mail notification has been sent", IO.EventLogInformation);

            }

            catch (Exception ex)
            {
                Console.WriteLine(" Mail  error is: " + ex);
                Console.WriteLine(ex.Message);
                IO.mWrite_To_EventLog("(Error Message) Email Mgr Module: " + ex.Message.ToString(), IO.EventLogError);

            }
        }
        //----------
        public static void mProcessMail(string EmailTo, string EmailFrom,string mMessage)
        {
            try
            {
                DateTime DateTime = DateTime.Now;

                string mMailBody = " \r\n ";
                mMailBody = mMailBody + "Esker Processing Error:  " + DateTime.ToLongDateString() + " - " + DateTime.ToLongTimeString();
                mMailBody = mMailBody + " \r\n \r\n\r\n \r\n";

                mMailBody = mMailBody + "   ...    " + mMessage;

                mMailBody = mMailBody + " \r\n \r\n\r\n \r\n";
                mMailBody = mMailBody + " Esker";
                mMailBody = mMailBody + " \r\n \r\n\r\n \r\n";


                mMailBody = mMailBody + "\r\n\t\t\t                                                       ";
                mMailBody = mMailBody + "\r\n\t\t\t                    FedEx Rate Checker Processing \r\n \r\n\r\n ";
                mMailBody = mMailBody + "\r\n\t\t    © 2008 CarSound Exhaust Systems, Inc. All rights reserved.";

                MailMessage mail = new MailMessage();

                mail.To     = EmailTo;
                mail.From   = EmailFrom; // "pjones@magnaflow.com";
                mail.Bcc    = "pjones@magnaflow.com";
                mail.Subject = "FedEx Rate Check: " + DateTime.ToLongDateString() + " - " + DateTime.ToLongTimeString();
                mail.Body = mMailBody;

                SmtpMail.SmtpServer = "CSAVSMAIL1.magnaflow.com";
                SmtpMail.Send(mail);

                IO.mWrite_To_EventLog("Email Mgr Module: Mail notification has been sent (Signature 2)", IO.EventLogInformation);

            }

            catch (Exception ex)
            {
                Console.WriteLine(" Mail  error is: " + ex);
                Console.WriteLine(ex.Message);
                IO.mWrite_To_EventLog("(Error Message) Email Mgr Module: " + ex.Message.ToString(), IO.EventLogError);

            }
        }

        //----------
        // Report
        //----------
        public static void MailIssuesList(string EmailTo, string EmailFrom, ArrayList Message, FrontDoor.Document_Table HF)
        {
            try
            {                
                string mMessage = " " ;
                string mMailBody = " \r\n ";

                DateTime DateTime    = DateTime.Now;
                IEnumerator mIterate = Message.GetEnumerator();  

                while (mIterate.MoveNext())
                {
                    ReportMgr.Report_Data_ MsgRecord = (ReportMgr.Report_Data_)mIterate.Current;

                    //
                    string NoteText_SurCharge = null;
                    if (MsgRecord.aMessage01.Contains(FrontDoor.Memo_Surcharge))
                    {
                        try
                        {
                            char[] S_Delim = FedEx_Rates.SurChargeDelim.ToCharArray();
                            string[] SurChargeArray = MsgRecord.aMessage01.Split(S_Delim);
                            foreach (string S_Charge in SurChargeArray)
                            {
                                if (S_Charge.Contains(FrontDoor.Memo_Surcharge))
                                {
                                    Console.WriteLine("%-% ");

                                    Console.WriteLine("%%  Index {0} Text {1}: ", S_Charge.IndexOf(FrontDoor.Memo_Surcharge), MsgRecord.aMessage01);
                                   

                                 //   int SurChrgNote_Start = S_Charge.IndexOf(FrontDoor.Memo_Start, Message.IndexOf(FrontDoor.Memo_Surcharge));
                                 //   Console.WriteLine("%% Start {0] Index {1} Text {2}: ", SurChrgNote_Start, Message.IndexOf(FrontDoor.Memo_Surcharge), MsgRecord.aMessage01);
                                    string S_ChargeNote = S_Charge;
                                    //            S_ChargeNote = S_ChargeNote.Substring(SurChrgNote_Start);
                                    S_ChargeNote = S_ChargeNote.Replace("SurCharge~", "");
                                    S_ChargeNote = S_ChargeNote.Replace("^","");
                                    S_ChargeNote = S_ChargeNote.Trim();
                                    NoteText_SurCharge = "  \r\n" + S_ChargeNote + "  \r\n";
                                }
                            }
                        }
                        catch (Exception ex)
                        {
                            Console.WriteLine("[IssuesList] Mail  error is: " + ex);
                            Console.WriteLine(ex.Message);
                            IO.mWrite_To_EventLog("(Error Message) Email Mgr Module: " + ex.Message.ToString(), IO.EventLogError);

                        }

                    }

                    //
                    mMessage = mMessage + "Order: " + MsgRecord.Order_no + "  Tracking: " + MsgRecord.tracking_number + "  \r\n";
                    mMessage = mMessage + "  Ship_via_code: " + MsgRecord.Ship_via_code + "  FedEx Service_Type: " + MsgRecord.Service_Type + "  \r\n";
                    mMessage = mMessage + "  IFS Charge: " + MsgRecord.line_package_charge + "  FedEx Base Charge: " + MsgRecord.Base_Charge + "  \r\n";
                    mMessage = mMessage + "  FedEx Total_Net_Charge: " + MsgRecord.Total_Net_Charge + "  \r\n";
                    mMessage = mMessage + "  Height: " + MsgRecord.actual_height + "  Width: " + MsgRecord.actual_width +
                                          "  Length: " + MsgRecord.actual_length + "  Weight: " + MsgRecord.actual_weight + "  \r\n";
                    mMessage = mMessage + "  FedEx Surcharge: " + MsgRecord.Surcharge + NoteText_SurCharge + "  \r\n\r\n";
                    mMessage = mMessage + "  .........................................................  \r\n\r\n";

                }



                //string mMailBody = " \r\n ";
                mMailBody = mMailBody + "FedEx Rate Processing:  " + DateTime.ToLongDateString() + " - " + DateTime.ToLongTimeString();
                mMailBody = mMailBody + " \r\n \r\n";
                mMailBody = mMailBody + " There were " + HF.aErrorCount.ToString() +" possible variance(s) out of " +
                                        HF.aBatchCount + " shipments. \r\n \r\n";
                mMailBody = mMailBody + " \r\n \r\n";
                mMailBody = mMailBody + " FedEx shipment details to review..." + " \r\n ";
                mMailBody = mMailBody + " \r\n \r\n\r\n \r\n";

                mMailBody = mMailBody + mMessage;

                mMailBody = mMailBody + " \r\n \r\n\r\n \r\n";
                mMailBody = mMailBody + " \r\n \r\n\r\n \r\n";


                mMailBody = mMailBody + "\r\n\t\t\t                                                       ";
                mMailBody = mMailBody + "\r\n\t\t\t                    FedEx Rate Checker Processing \r\n \r\n\r\n ";
                mMailBody = mMailBody + "\r\n\t\t    © 2011 CarSound Exhaust Systems, Inc. All rights reserved.";

                MailMessage mail = new MailMessage();

                mail.To = EmailTo;
                mail.From = EmailFrom; // "pjones@magnaflow.com";
                mail.Bcc = "pjones@magnaflow.com";
                mail.Subject = "FedEx Rate Check: " + DateTime.ToLongDateString() + " - " + DateTime.ToLongTimeString();
                mail.Body = mMailBody;

                SmtpMail.SmtpServer = "CSAVSMAIL1.magnaflow.com";
                SmtpMail.Send(mail);

                IO.mWrite_To_EventLog("Email Mgr Module: Mail notification has been sent (Signature 2)", IO.EventLogInformation);

            }

            catch (Exception ex)
            {
                Console.WriteLine(" Mail  error is: " + ex);
                Console.WriteLine(ex.Message);
                IO.mWrite_To_EventLog("(Error Message) Email Mgr Module: " + ex.Message.ToString(), IO.EventLogError);

            }
        }

        //----------
        // Alert: Issue
        //----------
        public static void MailAlert(string EmailTo, string EmailFrom, string Message)
        {
            try
            {

                string mMessage = Message;
                string mMailBody = " \r\n ";

                DateTime DateTime = DateTime.Now;
                

                //string mMailBody = " \r\n ";
                mMailBody = mMailBody + "Error Alert:  " + DateTime.ToLongDateString() + " - " + DateTime.ToLongTimeString();
                mMailBody = mMailBody + " \r\n \r\n";
                mMailBody = mMailBody + " Something is wrong..." + " \r\n ";
                mMailBody = mMailBody + " \r\n \r\n\r\n \r\n";

                mMailBody = mMailBody + mMessage;

                mMailBody = mMailBody + " \r\n \r\n\r\n \r\n";
                mMailBody = mMailBody + " \r\n \r\n\r\n \r\n";


                mMailBody = mMailBody + "\r\n\t\t\t                                                       ";
                mMailBody = mMailBody + "\r\n\t\t\t                    FedEx Rate Checker Processing \r\n \r\n\r\n ";
                mMailBody = mMailBody + "\r\n\t\t    © 2011 CarSound Exhaust Systems, Inc. All rights reserved.";

                MailMessage mail = new MailMessage();

                mail.To = EmailTo;
                mail.From = EmailFrom; // "pjones@magnaflow.com";
                mail.Bcc = "pjones@magnaflow.com";
                mail.Subject = "FedEx Rate Check: " + DateTime.ToLongDateString() + " - " + DateTime.ToLongTimeString();
                mail.Body = mMailBody;

                SmtpMail.SmtpServer = "CSAVSMAIL1.magnaflow.com";
                SmtpMail.Send(mail);

                IO.mWrite_To_EventLog("Email Mgr Module: Mail notification has been sent (Signature 3: Alert)", IO.EventLogInformation);

            }

            catch (Exception ex)
            {
                Console.WriteLine(" Mail  error is: " + ex);
                Console.WriteLine(ex.Message);
                IO.mWrite_To_EventLog("(Error Message) Email Mgr Module: " + ex.Message.ToString(), IO.EventLogError);

            }
        }


    }
}


