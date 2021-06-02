/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 * @author: not specified
*/
grammar Claw;

@header
{
import claw.wani.language.ClawLanguage;
}

/*----------------------------------------------------------------------------
 * PARSER RULES
 *----------------------------------------------------------------------------*/
analyze returns [ClawLanguage l]
  @init{
    $l = new ClawLanguage();
  }
  :
    CLAW ACC EOF
    { $l.setDirective(ClawDirective.PRIMITIVE); }
  | CLAW OMP EOF
    { $l.setDirective(ClawDirective.PRIMITIVE); }
;

/*----------------------------------------------------------------------------
 * LEXER RULES
 *----------------------------------------------------------------------------*/
CLAW         : 'claw';
ACC          : 'acc';
OMP          : 'omp';
IDENTIFIER   : [a-zA-Z_$] [a-zA-Z_$0-9-]* ;
