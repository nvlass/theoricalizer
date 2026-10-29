(ns theoricalizer.i18n.en
  (:require [clojure.string :as str]
            [theoricalizer.theory :as t]))

(defn- pc [loc sp] (t/pc-name sp (:names loc)))

(defn- ordinal [n]
  (case n
    1 "unison"
    8 "octave"
    (str n (cond (#{11 12 13} n) "th" (= 1 (mod n 10)) "st" (= 2 (mod n 10)) "nd" (= 3 (mod n 10)) "rd" :else "th"))))

(def quality-words {:P "perfect" :M "major" :m "minor" :A "augmented" :d "diminished" :AA "doubly augmented" :dd "doubly diminished"})
(def scale-words {:major "major" :natural "natural minor" :harmonic "harmonic minor" :melodic "melodic minor"})
(def triad-words {:maj "major" :min "minor" :dim "diminished" :aug "augmented"})
(def inv-words ["root position" "first inversion" "second inversion"])

(defn- iv [_ {:keys [num q]}] (str (quality-words q) " " (ordinal num)))
(defn- key-name [loc {:keys [tonic mode]}] (str (pc loc tonic) " " (name mode)))

(def ^:private base
  {:nav {:practice "Practice" :intervals "Intervals" :scales "Scales" :triads "Triads"}

   :kind {:interval-name "Name" :interval-write "Write" :interval-keys "Which keys"
          :scale-name "Name" :scale-write "Write"
          :triad-name "Name" :triad-write "Write" :triad-keys "Which keys"}

   :fmt
   {:quality (fn [_ q] (str/capitalize (quality-words q)))
    :iv iv
    :iv-short (fn [_ {:keys [num q]}] (str ({:P "P" :M "M" :m "m" :A "A" :d "d" :AA "AA" :dd "dd"} q) num))
    :note (fn [loc n] (t/note-name n (:names loc)))
    :pc pc
    :key key-name
    :scale-type (fn [_ ty] (str/capitalize (scale-words ty)))
    :scale (fn [loc tonic ty] (str (pc loc tonic) " " (scale-words ty)))
    :triad-quality (fn [_ q] (str/capitalize (triad-words q)))
    :triad (fn [loc root q inv] (str (pc loc root) " " (triad-words q) " triad, " (inv-words inv)))
    :inv (fn [_ i] (str/capitalize (inv-words i)))
    :forms (fn [_ types] (str/join ", " (map scale-words types)))}

   :ui
   {:options "Options"
    :clef "Clef" :treble "Treble" :bass "Bass" :both "Both"
    :max-acc "Keys"
    :up-to (fn [_ n] (str "up to " n " ♯/♭"))
    :minor-forms "Minor scales"
    :forms-harmonic "Harmonic"
    :forms-all "All three forms"
    :forms-hint "Which minor scales count when you look for the keys of an interval or a triad."
    :aug-dim "Augmented and diminished intervals"
    :compound "Compound intervals (beyond the octave)"
    :with-sig "Write scales with a key signature"
    :with-sig-hint "otherwise every sharp and flat is written as an accidental"
    :inversions "Triads in inversion"
    :note-names "Note names"
    :solfege-sample "Do Re Mi"
    :keys-major "Major"
    :keys-minor "Minor"
    :selected (fn [_ nm] (str "Selected: " nm))
    :click-staff "Click the staff to place a note."
    :up "Up a step" :down "Down a step" :delete "Delete"
    :correct "✓ Correct"
    :wrong "✗ Not quite"
    :it-is (fn [_ text semis] (str "Answer: " text (when semis (str " (" semis " semitones)"))))
    :keys-answer "The keys:"
    :task-interval-name "Name the interval."
    :task-scale-name "Name the scale."
    :task-triad-name "Name the triad: root, quality and position."
    :task-interval-write (fn [loc iv dir] (str "Write a " (iv loc iv) (if (= dir :up) " above" " below") " the given note."))
    :task-scale-write (fn [loc tonic ty sig?] (str "Write the " (pc loc (t/spelling tonic)) " " (scale-words ty) " scale, ascending"
                                                   (when sig? ", with its key signature") "."))
    :task-triad-write (fn [loc root q inv] (str "Write the " (pc loc root) " " (triad-words q) " triad in " (inv-words inv) "."))
    :task-interval-keys (fn [_ forms] (str "In which keys does this interval appear? ("
                                           (if (= forms :all) "major, and minor in all three forms" "major and harmonic minor") ")"))
    :task-triad-keys (fn [_ forms] (str "In which keys does this triad appear? ("
                                        (if (= forms :all) "major, and minor in all three forms" "major and harmonic minor") ")"))
    :play "▶ Play"
    :play-right "▶ The right answer"
    :next "Next →"
    :check "Check"
    :keys-hint "Enter: check / next · Space: play"
    :keys-write "↑ ↓ move the note · + − accidental · ← → column · Backspace delete · Enter check"}

   :lesson
   {:th-interval "Interval" :th-semis "Semitones" :th-example "Example"
    :th-signature "Signature" :th-major "Major" :th-minor "Minor"
    :degree-name (fn [_ d] (["Tonic" "Supertonic" "Mediant" "Subdominant" "Dominant" "Submediant"
                             "Leading tone (subtonic when it is a whole tone below the tonic, as in natural minor)"] d))

    :iv-what-title "What an interval is"
    :iv-what [:<> [:p "An interval is the distance between two notes. Played one after the other it is "
                   [:b "melodic"] "; together, " [:b "harmonic"] "."]
              [:p "Its " [:b "number"] " counts the letter names from one note to the other, both included: C–E is a 3rd (C D E), C–C a unison (1st), C up to the next C an octave (8th). Sharps and flats don't change the number: C–E♭ and C♯–E are also 3rds."]]

    :iv-quality-title "Quality: counting semitones"
    :iv-quality [:p "The number alone doesn't say how big an interval is: C–E is 4 semitones, C–E♭ is 3. The "
                 [:b "quality"] " does. Here are the intervals up to the octave, from C:"]

    :iv-families-title "Two families"
    :iv-families [:<> [:p [:b "1sts, 4ths, 5ths and 8ves"] " are " [:b "perfect"] " in their usual size. One semitone bigger they are " [:b "augmented"] ", one smaller " [:b "diminished"] "."]
                  [:p [:b "2nds, 3rds, 6ths and 7ths"] " come as " [:b "major"] " or " [:b "minor"] " (a semitone smaller). One semitone bigger than major is " [:b "augmented"] "; one smaller than minor is " [:b "diminished"] "."]
                  [:p.formula "d ← P → A      d ← m ← M → A"]]

    :iv-method-title "Naming an interval"
    :iv-method [:<> [:p "1. Count the letters for the number. 2. Imagine the major scale of the lower note: its notes are major (2, 3, 6, 7) or perfect (1, 4, 5, 8) above the tonic. 3. Compare the upper note with that scale note and adjust."]
                [:p "F–B: a 4th. F major has B♭, so F–B♭ is a perfect 4th, and B is a semitone higher: an augmented 4th."]
                [:p "If the lower note has a sharp or a flat, name the interval without it first, then adjust: F♯–A is a semitone smaller than F–A (major 3rd), so it is a minor 3rd."]]

    :iv-inversion-title "Inversion"
    :iv-inversion [:p "Moving the lower note up an octave (or the upper one down) inverts an interval. The two numbers add up to 9, and the quality turns over: major ↔ minor, augmented ↔ diminished, perfect stays perfect. A major 3rd becomes a minor 6th; an augmented 4th a diminished 5th."]

    :iv-compound-title "Compound intervals"
    :iv-compound [:p "Intervals larger than an octave are compound: a 9th is a 2nd plus an octave, a 10th a 3rd plus an octave, and so on (subtract 7). They keep the quality of the simple interval: C–D an octave higher is a major 9th."]

    :iv-enharmonic-title "Enharmonic intervals"
    :iv-enharmonic [:p "Two intervals can sound the same and still be different, because they are spelled differently: C–F♯ (augmented 4th) and C–G♭ (diminished 5th) are both 6 semitones; C–D♯ (augmented 2nd) and C–E♭ (minor 3rd) are both 3. The spelling decides the name."]

    :iv-keys-title "In which keys does an interval appear?"
    :iv-keys [:<> [:p "Look for the keys whose scale contains both notes, spelled exactly as written. A quick way: list the keys that contain the more unusual note (the one with a sharp or flat), then keep those that also contain the other note. Major and perfect intervals turn up in many keys; augmented and diminished ones in few."]
              [:p "F–B (augmented 4th), in major and harmonic minor keys, with the degrees of the two notes:"]]
    :iv-keys-harmonic [:p "Four intervals exist only in harmonic minor, because of its raised 7th: the augmented 2nd (6–7) and its inversion the diminished 7th (7–6), and the augmented 5th (3–7) and its inversion the diminished 4th (7–3). In A minor:"]

    :sc-major-title "The major scale"
    :sc-major [:<> [:p "Between neighbouring letters there is a whole tone, except E–F and B–C, which are semitones. A " [:b "major scale"] " follows the pattern"]
               [:p.formula "tone · tone · semitone · tone · tone · tone · semitone"]
               [:p "so the semitones fall between degrees 3–4 and 7–8. On C it needs no sharps or flats; on any other note some are needed to keep the pattern, as F major needs B♭."]]

    :sc-signature-title "Key signatures"
    :sc-signature [:<> [:p "The sharps or flats a key needs are written once, at the start of every line: the " [:b "key signature"] ". They always appear in the same order:"]
                   [:p.formula "sharps: F C G D A E B      flats: B E A D G C F"]
                   [:p "With sharps, the major key is a semitone above the last sharp (F♯ C♯ → D major). With flats, it is the second-to-last flat (B♭ E♭ A♭ → E♭ major); one flat is F major."]]

    :sc-minor-title "The three minor scales"
    :sc-minor [:<> [:p "Every major key has a " [:b "relative minor"] " a minor 3rd below, with the same key signature (C major – A minor). The minor scale comes in three forms:"]
               [:ul [:li [:b "Natural:"] " the notes of the signature, nothing added."]
                [:li [:b "Harmonic:"] " the 7th degree is raised a semitone, so it leads to the tonic. This leaves an augmented 2nd between degrees 6 and 7. The raised note is always written as an accidental."]
                [:li [:b "Melodic:"] " going up, the 6th and 7th are both raised (no augmented 2nd); coming down, it is the natural minor."]]]

    :sc-degrees-title "Degree names"
    :sc-degrees [:p "Each degree of a scale has a name, which says something about its role:"]

    :sc-related-title "Related keys"
    :sc-related [:<> [:p [:b "Relative"] " keys share a signature (G major – E minor). " [:b "Parallel"] " keys share a tonic (C major – C minor)."]
                 [:p "Going up a perfect 5th adds a sharp (or removes a flat): C G D A E B F♯ C♯. Going down a 5th adds a flat: C F B♭ E♭ A♭ D♭ G♭ C♭. This is the " [:b "circle of fifths"] "; at the bottom, F♯ and G♭ (6 sharps / 6 flats) are the same keys, enharmonically."]]}})

(def ^:private triad-lessons
  {:tr-what-title "What a triad is"
   :tr-what [:<> [:p "A triad is three notes stacked in thirds: the " [:b "root"] ", the " [:b "third"] " and the " [:b "fifth"] ". Its quality comes from the two thirds:"]
             [:ul [:li [:b "Major:"] " major 3rd + minor 3rd (perfect 5th from the root)."]
              [:li [:b "Minor:"] " minor 3rd + major 3rd (perfect 5th)."]
              [:li [:b "Diminished:"] " minor 3rd + minor 3rd (diminished 5th)."]
              [:li [:b "Augmented:"] " major 3rd + major 3rd (augmented 5th)."]]]

   :tr-degrees-title "Triads on each degree"
   :tr-degrees [:p "Build a triad on every degree of a scale, using only its notes, and you get a fixed pattern of qualities. Roman numerals name them: capitals for major, small letters for minor, ° for diminished, + for augmented."]

   :tr-inversions-title "Inversions"
   :tr-inversions [:p "The lowest note decides the position. With the root in the bass the triad is in " [:b "root position"] " (5/3). With the third in the bass it is in " [:b "first inversion"] " (6, a \"sixth chord\"); with the fifth in the bass, " [:b "second inversion"] " (6/4). The other notes can be in any order and octave."]

   :tr-keys-title "In which keys does a triad appear?"
   :tr-keys [:p "A triad belongs to a key when all three notes are in its scale. Its quality tells you which degrees to try:"]})

(def dict (update base :lesson merge triad-lessons))
