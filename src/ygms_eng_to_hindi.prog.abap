*&---------------------------------------------------------------------*
*& Report YGMS_ENG_TO_HINDI
*&---------------------------------------------------------------------*
*& Description: English to Hindi Text Converter
*& This program translates English words and sentences into Hindi
*& using a dictionary-based lookup approach with Devanagari Unicode.
*&---------------------------------------------------------------------*
REPORT ygms_eng_to_hindi.

*----------------------------------------------------------------------*
* Types
*----------------------------------------------------------------------*
TYPES: BEGIN OF ty_dictionary,
         english TYPE string,
         hindi   TYPE string,
       END OF ty_dictionary,
       tt_dictionary TYPE SORTED TABLE OF ty_dictionary
                     WITH UNIQUE KEY english.

*----------------------------------------------------------------------*
* Global Data
*----------------------------------------------------------------------*
DATA: gt_dictionary TYPE tt_dictionary.

*----------------------------------------------------------------------*
* Selection Screen
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS:
    p_input TYPE string LOWER CASE OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  PARAMETERS:
    rb_word RADIOBUTTON GROUP mode DEFAULT 'X',
    rb_sent RADIOBUTTON GROUP mode.
SELECTION-SCREEN END OF BLOCK b2.

*----------------------------------------------------------------------*
* Initialization
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM build_dictionary.

*----------------------------------------------------------------------*
* Start of Selection
*----------------------------------------------------------------------*
START-OF-SELECTION.
  DATA: lv_result TYPE string.

  IF rb_word = abap_true.
    " Single word translation
    PERFORM translate_word USING p_input
                           CHANGING lv_result.
  ELSE.
    " Full sentence translation (word-by-word)
    PERFORM translate_sentence USING p_input
                               CHANGING lv_result.
  ENDIF.

*----------------------------------------------------------------------*
* End of Selection - Display Output
*----------------------------------------------------------------------*
END-OF-SELECTION.
  PERFORM display_output USING p_input lv_result.

*&---------------------------------------------------------------------*
*& Form BUILD_DICTIONARY
*&---------------------------------------------------------------------*
*& Populates the English-Hindi dictionary with common words
*&---------------------------------------------------------------------*
FORM build_dictionary.

  DATA: ls_entry TYPE ty_dictionary.

  DEFINE add_word.
    ls_entry-english = &1.
    ls_entry-hindi   = &2.
    INSERT ls_entry INTO TABLE gt_dictionary.
  END-OF-DEFINITION.

  " --- Greetings & Common Phrases ---
  add_word 'hello'        'नमस्ते'.
  add_word 'goodbye'      'अलविदा'.
  add_word 'good'         'अच्छा'.
  add_word 'morning'      'सुबह'.
  add_word 'evening'      'शाम'.
  add_word 'night'        'रात'.
  add_word 'welcome'      'स्वागत'.
  add_word 'thanks'       'धन्यवाद'.
  add_word 'thank'        'धन्यवाद'.
  add_word 'please'       'कृपया'.
  add_word 'sorry'        'क्षमा'.
  add_word 'yes'          'हाँ'.
  add_word 'no'           'नहीं'.
  add_word 'ok'           'ठीक'.
  add_word 'bye'          'अलविदा'.
  add_word 'hi'           'नमस्ते'.
  add_word 'namaste'      'नमस्ते'.

  " --- Pronouns ---
  add_word 'i'            'मैं'.
  add_word 'you'          'तुम'.
  add_word 'he'           'वह'.
  add_word 'she'          'वह'.
  add_word 'it'           'यह'.
  add_word 'we'           'हम'.
  add_word 'they'         'वे'.
  add_word 'this'         'यह'.
  add_word 'that'         'वह'.
  add_word 'my'           'मेरा'.
  add_word 'your'         'तुम्हारा'.
  add_word 'his'          'उसका'.
  add_word 'her'          'उसकी'.
  add_word 'our'          'हमारा'.
  add_word 'their'        'उनका'.
  add_word 'me'           'मुझे'.
  add_word 'him'          'उसे'.
  add_word 'us'           'हमें'.
  add_word 'them'         'उन्हें'.
  add_word 'who'          'कौन'.
  add_word 'what'         'क्या'.
  add_word 'where'        'कहाँ'.
  add_word 'when'         'कब'.
  add_word 'why'          'क्यों'.
  add_word 'how'          'कैसे'.
  add_word 'which'        'कौन सा'.

  " --- Verbs ---
  add_word 'is'           'है'.
  add_word 'am'           'हूँ'.
  add_word 'are'          'हैं'.
  add_word 'was'          'था'.
  add_word 'were'         'थे'.
  add_word 'have'         'है'.
  add_word 'has'          'है'.
  add_word 'had'          'था'.
  add_word 'do'           'करना'.
  add_word 'does'         'करता है'.
  add_word 'did'          'किया'.
  add_word 'go'           'जाना'.
  add_word 'going'        'जा रहा'.
  add_word 'gone'         'गया'.
  add_word 'come'         'आना'.
  add_word 'coming'       'आ रहा'.
  add_word 'came'         'आया'.
  add_word 'eat'          'खाना'.
  add_word 'eating'       'खा रहा'.
  add_word 'drink'        'पीना'.
  add_word 'drinking'     'पी रहा'.
  add_word 'sleep'        'सोना'.
  add_word 'sleeping'     'सो रहा'.
  add_word 'walk'         'चलना'.
  add_word 'walking'      'चल रहा'.
  add_word 'run'          'दौड़ना'.
  add_word 'running'      'दौड़ रहा'.
  add_word 'read'         'पढ़ना'.
  add_word 'reading'      'पढ़ रहा'.
  add_word 'write'        'लिखना'.
  add_word 'writing'      'लिख रहा'.
  add_word 'speak'        'बोलना'.
  add_word 'speaking'     'बोल रहा'.
  add_word 'listen'       'सुनना'.
  add_word 'listening'    'सुन रहा'.
  add_word 'see'          'देखना'.
  add_word 'seeing'       'देख रहा'.
  add_word 'give'         'देना'.
  add_word 'giving'       'दे रहा'.
  add_word 'take'         'लेना'.
  add_word 'taking'       'ले रहा'.
  add_word 'make'         'बनाना'.
  add_word 'making'       'बना रहा'.
  add_word 'know'         'जानना'.
  add_word 'think'        'सोचना'.
  add_word 'want'         'चाहना'.
  add_word 'need'         'जरूरत'.
  add_word 'like'         'पसंद'.
  add_word 'love'         'प्यार'.
  add_word 'work'         'काम'.
  add_word 'working'      'काम कर रहा'.
  add_word 'play'         'खेलना'.
  add_word 'playing'      'खेल रहा'.
  add_word 'sit'          'बैठना'.
  add_word 'sitting'      'बैठ रहा'.
  add_word 'stand'        'खड़ा होना'.
  add_word 'standing'     'खड़ा'.
  add_word 'open'         'खोलना'.
  add_word 'close'        'बंद करना'.
  add_word 'buy'          'खरीदना'.
  add_word 'sell'         'बेचना'.
  add_word 'send'         'भेजना'.
  add_word 'receive'      'प्राप्त करना'.
  add_word 'wait'         'इंतजार करना'.
  add_word 'stop'         'रुकना'.
  add_word 'start'        'शुरू करना'.
  add_word 'finish'       'समाप्त करना'.
  add_word 'help'         'मदद'.
  add_word 'try'          'कोशिश करना'.
  add_word 'learn'        'सीखना'.
  add_word 'teach'        'सिखाना'.
  add_word 'live'         'जीना'.
  add_word 'die'          'मरना'.
  add_word 'can'          'सकता'.
  add_word 'will'         'होगा'.
  add_word 'would'        'होगा'.
  add_word 'should'       'चाहिए'.
  add_word 'could'        'सकता था'.
  add_word 'must'         'जरूर'.
  add_word 'not'          'नहीं'.

  " --- Nouns - People & Relations ---
  add_word 'name'         'नाम'.
  add_word 'person'       'व्यक्ति'.
  add_word 'man'          'आदमी'.
  add_word 'woman'        'औरत'.
  add_word 'boy'          'लड़का'.
  add_word 'girl'         'लड़की'.
  add_word 'child'        'बच्चा'.
  add_word 'children'     'बच्चे'.
  add_word 'father'       'पिता'.
  add_word 'mother'       'माता'.
  add_word 'brother'      'भाई'.
  add_word 'sister'       'बहन'.
  add_word 'son'          'बेटा'.
  add_word 'daughter'     'बेटी'.
  add_word 'husband'      'पति'.
  add_word 'wife'         'पत्नी'.
  add_word 'friend'       'दोस्त'.
  add_word 'teacher'      'शिक्षक'.
  add_word 'student'      'छात्र'.
  add_word 'doctor'       'चिकित्सक'.
  add_word 'king'         'राजा'.
  add_word 'queen'        'रानी'.
  add_word 'people'       'लोग'.
  add_word 'family'       'परिवार'.

  " --- Nouns - Nature & Places ---
  add_word 'water'        'पानी'.
  add_word 'fire'         'आग'.
  add_word 'air'          'हवा'.
  add_word 'earth'        'पृथ्वी'.
  add_word 'sky'          'आकाश'.
  add_word 'sun'          'सूरज'.
  add_word 'moon'         'चाँद'.
  add_word 'star'         'तारा'.
  add_word 'rain'         'बारिश'.
  add_word 'river'        'नदी'.
  add_word 'mountain'     'पहाड़'.
  add_word 'tree'         'पेड़'.
  add_word 'flower'       'फूल'.
  add_word 'fruit'        'फल'.
  add_word 'animal'       'जानवर'.
  add_word 'bird'         'पक्षी'.
  add_word 'fish'         'मछली'.
  add_word 'dog'          'कुत्ता'.
  add_word 'cat'          'बिल्ली'.
  add_word 'horse'        'घोड़ा'.
  add_word 'cow'          'गाय'.
  add_word 'lion'         'शेर'.
  add_word 'elephant'     'हाथी'.
  add_word 'house'        'घर'.
  add_word 'home'         'घर'.
  add_word 'city'         'शहर'.
  add_word 'village'      'गाँव'.
  add_word 'country'      'देश'.
  add_word 'world'        'दुनिया'.
  add_word 'road'         'सड़क'.
  add_word 'school'       'विद्यालय'.
  add_word 'market'       'बाजार'.
  add_word 'garden'       'बगीचा'.
  add_word 'temple'       'मंदिर'.
  add_word 'office'       'कार्यालय'.

  " --- Nouns - Body & Food ---
  add_word 'body'         'शरीर'.
  add_word 'head'         'सिर'.
  add_word 'eye'          'आँख'.
  add_word 'eyes'         'आँखें'.
  add_word 'ear'          'कान'.
  add_word 'nose'         'नाक'.
  add_word 'mouth'        'मुँह'.
  add_word 'hand'         'हाथ'.
  add_word 'foot'         'पैर'.
  add_word 'heart'        'दिल'.
  add_word 'food'         'भोजन'.
  add_word 'milk'         'दूध'.
  add_word 'bread'        'रोटी'.
  add_word 'rice'         'चावल'.
  add_word 'sugar'        'चीनी'.
  add_word 'salt'         'नमक'.
  add_word 'tea'          'चाय'.

  " --- Nouns - Time ---
  add_word 'time'         'समय'.
  add_word 'day'          'दिन'.
  add_word 'today'        'आज'.
  add_word 'tomorrow'     'कल'.
  add_word 'yesterday'    'कल'.
  add_word 'year'         'साल'.
  add_word 'month'        'महीना'.
  add_word 'week'         'सप्ताह'.
  add_word 'hour'         'घंटा'.
  add_word 'minute'       'मिनट'.
  add_word 'second'       'सेकंड'.
  add_word 'now'          'अब'.
  add_word 'then'         'तब'.
  add_word 'always'       'हमेशा'.
  add_word 'never'        'कभी नहीं'.
  add_word 'sometimes'    'कभी कभी'.
  add_word 'often'        'अक्सर'.

  " --- Days of the Week ---
  add_word 'monday'       'सोमवार'.
  add_word 'tuesday'      'मंगलवार'.
  add_word 'wednesday'    'बुधवार'.
  add_word 'thursday'     'गुरुवार'.
  add_word 'friday'       'शुक्रवार'.
  add_word 'saturday'     'शनिवार'.
  add_word 'sunday'       'रविवार'.

  " --- Adjectives ---
  add_word 'big'          'बड़ा'.
  add_word 'small'        'छोटा'.
  add_word 'new'          'नया'.
  add_word 'old'          'पुराना'.
  add_word 'young'        'जवान'.
  add_word 'long'         'लंबा'.
  add_word 'short'        'छोटा'.
  add_word 'tall'         'लंबा'.
  add_word 'hot'          'गरम'.
  add_word 'cold'         'ठंडा'.
  add_word 'fast'         'तेज'.
  add_word 'slow'         'धीमा'.
  add_word 'easy'         'आसान'.
  add_word 'hard'         'कठिन'.
  add_word 'happy'        'खुश'.
  add_word 'sad'          'दुखी'.
  add_word 'beautiful'    'सुंदर'.
  add_word 'ugly'         'बदसूरत'.
  add_word 'rich'         'अमीर'.
  add_word 'poor'         'गरीब'.
  add_word 'strong'       'मजबूत'.
  add_word 'weak'         'कमजोर'.
  add_word 'right'        'सही'.
  add_word 'wrong'        'गलत'.
  add_word 'true'         'सच'.
  add_word 'false'        'झूठ'.
  add_word 'clean'        'साफ'.
  add_word 'dirty'        'गंदा'.
  add_word 'full'         'भरा'.
  add_word 'empty'        'खाली'.
  add_word 'white'        'सफेद'.
  add_word 'black'        'काला'.
  add_word 'red'          'लाल'.
  add_word 'blue'         'नीला'.
  add_word 'green'        'हरा'.
  add_word 'yellow'       'पीला'.

  " --- Numbers ---
  add_word 'one'          'एक'.
  add_word 'two'          'दो'.
  add_word 'three'        'तीन'.
  add_word 'four'         'चार'.
  add_word 'five'         'पाँच'.
  add_word 'six'          'छह'.
  add_word 'seven'        'सात'.
  add_word 'eight'        'आठ'.
  add_word 'nine'         'नौ'.
  add_word 'ten'          'दस'.
  add_word 'hundred'      'सौ'.
  add_word 'thousand'     'हजार'.
  add_word 'million'      'दस लाख'.
  add_word 'zero'         'शून्य'.
  add_word 'first'        'पहला'.
  add_word 'last'         'आखिरी'.

  " --- Prepositions & Conjunctions ---
  add_word 'in'           'में'.
  add_word 'on'           'पर'.
  add_word 'at'           'पर'.
  add_word 'to'           'को'.
  add_word 'from'         'से'.
  add_word 'with'         'के साथ'.
  add_word 'without'      'बिना'.
  add_word 'for'          'के लिए'.
  add_word 'of'           'का'.
  add_word 'by'           'द्वारा'.
  add_word 'up'           'ऊपर'.
  add_word 'down'         'नीचे'.
  add_word 'between'      'बीच'.
  add_word 'before'       'पहले'.
  add_word 'after'        'बाद'.
  add_word 'above'        'ऊपर'.
  add_word 'below'        'नीचे'.
  add_word 'near'         'पास'.
  add_word 'far'          'दूर'.
  add_word 'inside'       'अंदर'.
  add_word 'outside'      'बाहर'.
  add_word 'here'         'यहाँ'.
  add_word 'there'        'वहाँ'.
  add_word 'and'          'और'.
  add_word 'or'           'या'.
  add_word 'but'          'लेकिन'.
  add_word 'if'           'अगर'.
  add_word 'because'      'क्योंकि'.
  add_word 'so'           'इसलिए'.
  add_word 'very'         'बहुत'.
  add_word 'also'         'भी'.
  add_word 'only'         'केवल'.
  add_word 'some'         'कुछ'.
  add_word 'many'         'बहुत'.
  add_word 'all'          'सब'.
  add_word 'every'        'हर'.
  add_word 'more'         'अधिक'.
  add_word 'less'         'कम'.
  add_word 'again'        'फिर'.
  add_word 'still'        'अभी भी'.
  add_word 'just'         'बस'.
  add_word 'about'        'के बारे में'.

  " --- Abstract & Business ---
  add_word 'money'        'पैसा'.
  add_word 'price'        'कीमत'.
  add_word 'book'         'किताब'.
  add_word 'letter'       'पत्र'.
  add_word 'news'         'समाचार'.
  add_word 'question'     'सवाल'.
  add_word 'answer'       'उत्तर'.
  add_word 'problem'      'समस्या'.
  add_word 'solution'     'समाधान'.
  add_word 'idea'         'विचार'.
  add_word 'story'        'कहानी'.
  add_word 'truth'        'सच'.
  add_word 'life'         'जीवन'.
  add_word 'death'        'मृत्यु'.
  add_word 'god'          'भगवान'.
  add_word 'power'        'शक्ति'.
  add_word 'peace'        'शांति'.
  add_word 'war'          'युद्ध'.
  add_word 'language'     'भाषा'.
  add_word 'word'         'शब्द'.
  add_word 'india'        'भारत'.
  add_word 'hindi'        'हिंदी'.
  add_word 'english'      'अंग्रेजी'.

  " --- Common Phrases (as single entries) ---
  add_word 'how are you'      'आप कैसे हैं'.
  add_word 'i am fine'        'मैं ठीक हूँ'.
  add_word 'good morning'     'सुप्रभात'.
  add_word 'good night'       'शुभ रात्रि'.
  add_word 'good evening'     'शुभ संध्या'.
  add_word 'what is your name' 'आपका नाम क्या है'.
  add_word 'my name is'       'मेरा नाम है'.
  add_word 'thank you'        'धन्यवाद'.
  add_word 'i love you'       'मैं तुमसे प्यार करता हूँ'.
  add_word 'see you'          'फिर मिलेंगे'.
  add_word 'excuse me'        'क्षमा कीजिए'.
  add_word 'i am sorry'       'मुझे खेद है'.
  add_word 'no problem'       'कोई बात नहीं'.
  add_word 'come here'        'यहाँ आओ'.
  add_word 'go there'         'वहाँ जाओ'.
  add_word 'sit down'         'बैठ जाओ'.
  add_word 'stand up'         'खड़े हो जाओ'.
  add_word 'let us go'        'चलो चलते हैं'.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form TRANSLATE_WORD
*&---------------------------------------------------------------------*
*& Translates a single English word to Hindi
*&---------------------------------------------------------------------*
FORM translate_word USING    iv_word   TYPE string
                    CHANGING cv_result TYPE string.

  DATA: lv_lower TYPE string.

  lv_lower = to_lower( iv_word ).

  " Remove leading/trailing spaces
  CONDENSE lv_lower.

  " Look up in dictionary
  READ TABLE gt_dictionary INTO DATA(ls_entry)
    WITH TABLE KEY english = lv_lower.

  IF sy-subrc = 0.
    cv_result = ls_entry-hindi.
  ELSE.
    " Word not found - return original with note
    cv_result = |{ iv_word } [शब्दकोश में नहीं मिला]|.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form TRANSLATE_SENTENCE
*&---------------------------------------------------------------------*
*& Translates an English sentence to Hindi word-by-word.
*& First attempts phrase matching for multi-word expressions,
*& then falls back to individual word translation.
*&---------------------------------------------------------------------*
FORM translate_sentence USING    iv_sentence TYPE string
                        CHANGING cv_result   TYPE string.

  DATA: lv_lower     TYPE string,
        lv_remaining TYPE string,
        lv_word      TYPE string,
        lv_hindi     TYPE string,
        lv_matched   TYPE abap_bool,
        lt_words     TYPE TABLE OF string.

  lv_lower = to_lower( iv_sentence ).
  CONDENSE lv_lower.

  " First try to match the full input as a known phrase
  READ TABLE gt_dictionary INTO DATA(ls_phrase)
    WITH TABLE KEY english = lv_lower.
  IF sy-subrc = 0.
    cv_result = ls_phrase-hindi.
    RETURN.
  ENDIF.

  " Try matching progressively shorter phrases from the beginning
  SPLIT lv_lower AT space INTO TABLE lt_words.

  DATA: lv_idx       TYPE i VALUE 1,
        lv_num_words TYPE i.

  lv_num_words = lines( lt_words ).
  CLEAR cv_result.

  WHILE lv_idx <= lv_num_words.
    lv_matched = abap_false.

    " Try matching longest possible phrase starting at current position
    " Try 4-word, 3-word, 2-word phrases, then single word
    DATA(lv_max_len) = lv_num_words - lv_idx + 1.
    IF lv_max_len > 4.
      lv_max_len = 4.
    ENDIF.

    DATA: lv_try_len TYPE i.
    lv_try_len = lv_max_len.

    WHILE lv_try_len >= 2.
      " Build a phrase from lv_idx to lv_idx + lv_try_len - 1
      DATA: lv_phrase TYPE string.
      CLEAR lv_phrase.

      DATA: lv_j TYPE i.
      lv_j = lv_idx.
      WHILE lv_j < lv_idx + lv_try_len.
        DATA(lv_w) = VALUE #( lt_words[ lv_j ] OPTIONAL ).
        IF lv_phrase IS INITIAL.
          lv_phrase = lv_w.
        ELSE.
          lv_phrase = |{ lv_phrase } { lv_w }|.
        ENDIF.
        lv_j = lv_j + 1.
      ENDWHILE.

      READ TABLE gt_dictionary INTO DATA(ls_found)
        WITH TABLE KEY english = lv_phrase.
      IF sy-subrc = 0.
        " Phrase match found
        IF cv_result IS INITIAL.
          cv_result = ls_found-hindi.
        ELSE.
          cv_result = |{ cv_result } { ls_found-hindi }|.
        ENDIF.
        lv_idx = lv_idx + lv_try_len.
        lv_matched = abap_true.
        EXIT.
      ENDIF.

      lv_try_len = lv_try_len - 1.
    ENDWHILE.

    " If no phrase matched, translate single word
    IF lv_matched = abap_false.
      lv_word = VALUE #( lt_words[ lv_idx ] OPTIONAL ).

      PERFORM translate_word USING lv_word
                             CHANGING lv_hindi.

      IF cv_result IS INITIAL.
        cv_result = lv_hindi.
      ELSE.
        cv_result = |{ cv_result } { lv_hindi }|.
      ENDIF.
      lv_idx = lv_idx + 1.
    ENDIF.
  ENDWHILE.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form DISPLAY_OUTPUT
*&---------------------------------------------------------------------*
*& Displays the translation result in a formatted output
*&---------------------------------------------------------------------*
FORM display_output USING iv_english TYPE string
                          iv_hindi   TYPE string.

  ULINE.
  WRITE: / '  English to Hindi Translator - अंग्रेजी से हिंदी अनुवादक'.
  ULINE.
  SKIP.

  FORMAT COLOR COL_HEADING.
  WRITE: / '  Input (English / अंग्रेजी):'.
  FORMAT COLOR OFF.
  WRITE: / '  ', iv_english.
  SKIP.

  FORMAT COLOR COL_POSITIVE.
  WRITE: / '  Output (Hindi / हिंदी):'.
  FORMAT COLOR OFF.
  WRITE: / '  ', iv_hindi.
  SKIP.

  ULINE.
  FORMAT COLOR COL_TOTAL.
  WRITE: / '  Dictionary entries loaded:', lines( gt_dictionary ).
  FORMAT COLOR OFF.

  IF rb_sent = abap_true.
    WRITE: / '  Mode: Sentence Translation (वाक्य अनुवाद)'.
  ELSE.
    WRITE: / '  Mode: Word Translation (शब्द अनुवाद)'.
  ENDIF.
  ULINE.

ENDFORM.
