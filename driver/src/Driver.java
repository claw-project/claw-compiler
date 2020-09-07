package clawfc;

import claw.ClawX2T;

public class Driver
{
    public static void main(String[] args) throws Exception
    {
        System.out.println("This is clawfc");
        claw.ClawX2T.main(new String[]{"--help"});
    }
};