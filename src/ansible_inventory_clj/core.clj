(ns ansible-inventory-clj.core
  (:require [clojure.spec :as s]
            [clojure.spec.gen :as gen]
            [clojure.spec.test :as stest]
            [com.gfredericks.test.chuck.generators :as cg]
            [clojure.string :as str]))

;; This namespace facilitates generating a dynamic ansible
;; inventory as is described in this link:
;; http://docs.ansible.com/ansible/developing_inventory.html


;; ----------------------------------------
;; SSH

(s/def ::ip4_part (s/int-in 0 256))
(s/def ::ip4_addr (s/coll-of ::ip4_part
                             :kind vector?
                             :count 4))
;; (samples ::ip4_addr)


(s/def ::port (s/int-in 0 65536))

(def portable_filename_character_set
  #{\A \B \C \D \E \F \G \H \I \J \K \L \M \N \O \P \Q \R \S \T \U \V \W \X \Y \Z
    \a \b \c \d \e \f \g \h \i \j \k \l \m \n \o \p \q \r \s \t \u \v \w \x \y \z
    \0 \1 \2 \3 \4 \5 \6 \7 \8 \9 \. \_ \-})

(s/def ::username
  (s/with-gen
    (s/and string?
           #(not (str/blank? %))
           #(not= \- (get % 0))
           #(every? portable_filename_character_set %))
    #(cg/string-from-regex #"[A-Za-z0-9][A-Za-z0-9_-]{10}")))
;; (samples ::username)

(s/def ::hostname
  (s/with-gen
    (s/and string?
           #(not (str/blank? %))
           #(every? portable_filename_character_set %))
    #(cg/string-from-regex #"[A-Za-z0-9][A-Za-z0-9_-]{1,10}\.[A-Za-z0-9_-]{1,10}")))
;; (samples ::hostname)

(s/def ::ssh (s/cat :username ::username
                    :destination (s/or :ip4 ::ip4_addr
                                       :hostname ::hostname)
                    :port ::port))

;; (samples ::ssh 2)

;; ----------------------------------------
;; TARGETS (things that ansible can target)

(s/def ::target ::ssh)
;; (samples ::target 2)

(s/def ::var-value (s/or :s string?
                         :d double?
                         :i int?
                         :b boolean?))
;; (samples ::var-value)

(s/def ::vars
  (s/with-gen
    (s/map-of string? ::var-value)
    #(gen/map (gen/string-alphanumeric)
              (s/gen ::var-value)
              {:max-elements 4})))
;;(samples ::vars)

(s/def ::inv-targets (s/map-of ::target ::vars))
;; (samples ::inv-targets 2)

;; ----------------------------------------
;; GROUPS (of targets)

(s/def ::group-id (s/and string?
                         #(and (not (str/blank? %))
                               (not (str/includes? % ".")))))

;; child-groups is a collection of group-id's
;; thereby referring to other groups (of targets)
(s/def ::group-children (s/coll-of ::group-id
                                   :into #{}))

;; just a set of things that can be targeted, ie, targets
;; but belonging to the group
(s/def ::group-targets (s/coll-of ::target
                                  :into #{}))
;; (samples ::group-targets 2)

;; groups can be empty
;; groups may have no variables
;; groups don't have to have child groups
;; a group is literally just the name, ::group-id
(s/def ::group-info (s/keys :opt [::vars
                                  ::group-children
                                  ::group-targets]))

(s/def ::inv-groups (s/map-of ::group-id ::group-info))
;; (samples ::inv-groups 2)

;; ----------------------------------------

(s/def ::inventory (s/keys :req [::inv-targets]
                           :opt [::inv-groups
                                 ::vars]))

;; ----------------------------------------

(def empty-inventory {::inv-targets {}
                      ::inv-groups {}})
;; (s/valid? ::inventory empty-inventory)

;; ----------------------------------------

(s/fdef target->str
  :args (s/cat :t (s/spec ::target))
  :ret string?)

(defn target->str
  [t]
  (let [parts (s/conform ::target t)
        dest-name-part (-> parts :destination second)]
    (condp = (-> parts :destination first)
      :hostname dest-name-part
      :ip4 (str "ip4_" (str/join "_" dest-name-part)))))

;; ----------------------------------------

(s/fdef add-target
  :args (s/cat :inv ::inventory
               :target (s/spec ::target)
               :vars (s/? ::vars))
  :ret ::inventory)

(defn add-target
  "Registers the connection details for a host/target with the inventory. Merges the vars with what's there already."
  ([inv t] (add-target inv t {}))
  ([inv t vars]
   (update-in inv [::inv-targets] assoc
              t
              (merge 
               (get-in inv [::inv-targets t] {})
               vars))))

;; ----------------------------------------

(defn group->in-inv-afterwards
  "Checks that the group (arg) is in the inventory that is returned. (This function acts as part of an s/fdef)"
  [x]
  (-> x
      :ret
      ::inv-groups
      (contains? (->> x
                      :args
                      :group-id))))

(s/fdef add-group
  :args (s/cat :inv ::inventory
               :group-id ::group-id
               :vars (s/? ::vars))
  :ret ::inventory
  :fn #(and (group->in-inv-afterwards %)))

(defn add-group
  "Adds a group (by name/id) to the inventory, optionally with variables."
  ([inv group-id] (add-group inv group-id {}))
  ([inv group-id vars]
   (update-in inv [::inv-groups group-id ::vars]
              #(merge % vars))))

;; ----------------------------------------

(defn target->group->-spec-fn-target-in-group
  [x]
  (let [t (-> x :args :target)
        gid (-> x :args :gid)]
    (contains? (get-in x [:ret ::inv-groups gid ::group-targets])
               t)))

(s/fdef add-target-to-group
  :args (s/cat :inv ::inventory
               :target (s/spec ::target)
               :gid ::group-id)
  :ret ::inventory
  :fn #(and (target->group->-spec-fn-target-in-group %)))

(defn add-target-to-group
  "Adds the target to the group in an inventory. (Also ensures that the group and the target themselves are a proper part of the inventory.)"
  [inv target gid]
  (as-> inv $
    (add-target $ target)
    (add-group $ gid)
    (update-in $ [::inv-groups gid]
               #(let [g-info (get % ::group-targets #{})]
                  (assoc % ::group-targets
                         (conj g-info target))))))

;; ----------------------------------------

(s/def ::string-array
  (s/with-gen (s/coll-of string?
                         :into []
                         :distinct true)
    #(gen/vector (gen/string) 0 10)))
;; (samples ::string-array 3)

(s/def ::ansible-inventory-list-group-string-details
  (s/map-of #{"hosts" "vars" "children"}
            (s/or :hosts ::string-array
                  :vars ::vars
                  :children ::string-array)))
;; (samples ::ansible-inventory-list-group-string-details 2)

(s/def ::ansible-inventory-list-group-option
  (s/or :plain ::string-array
        :detail ::ansible-inventory-list-group-string-details))
;; (samples ::ansible-inventory-list-group-option 2)

(s/def ::ansible-inventory-list-data
  (s/map-of ::group-id ::ansible-inventory-list-group-option))
;; (samples ::ansible-inventory-list-data 2)

;; ----------------------------------------

(s/fdef group->ansible--list-format
  :args (s/cat :gi ::group-info)
  :ret ::ansible-inventory-list-group-option)

(defn group->ansible--list-format
  "Transforms a group (::group-info) into"
  [g]
  (let [targets  (get g ::group-targets #{})
        hosts    (->> targets
                      (map target->str)
                      (into #{})
                      (into []))
        vars     (::vars g)
        children (->> g
                      ::group-children
                      (into []))]
    (cond
      ;; empty array is valid for when there are no targets
      (= 0 (count hosts))
      []

      ;; if we have vars or children specified
      ;; we use the full detailed option
      (or vars children)
      (as-> (hash-map "hosts" hosts) $
        (if children (assoc $ "children" children) $)
        (if vars (assoc $ "vars" vars) $))

      ;; has no vars or children, can return string-array
      true hosts)))

;; ----------------------------------------

(s/fdef inv->ansible--list-groups
  :args (s/cat :inv ::inventory)
  :ret ::ansible-inventory-list-data)

(defn inv->ansible--list-groups
  "Export the inventory into the '--list' format for dynamic ansible inventory, but does not (yet) contain the \"_meta\" key."
  [inv]
  (or 
   (->> (::inv-groups inv)
        (map (fn [[k v]]
               (hash-map (name k)
                         (group->ansible--list-format v))))
        (apply merge))
   {}))

;; ----------------------------------------

(s/fdef make-ansible-host
  :args (s/cat :inv ::inventory
               :host string?)
  :ret ::vars)

(defn make-ansible-host
  "Returns the host variables from an inventory for a host as specified by a string representation"
  [inv host-str]
  (or 
   (let [h (->> inv
                ::inv-targets
                keys
                (filter #(= host-str (target->str %)))
                first)]
     (get-in inv [::inv-targets h]))
   {}))

;; ----------------------------------------

(s/fdef inv->ansible-hostvars
  :args (s/cat :inv ::inventory)
  :ret (s/map-of string?
                 ::vars))

(defn inv->ansible-hostvars
  "Returns a map of hostnames-to-vars, suitable for the _meta[\"hostvars\" ansible inventory script. Does not include a key for \"_meta\" or for \"hostvars\". The calling site has to do that."
  [inv]
  (or 
   (->> inv
        ::inv-targets
        (map (fn [[h v]]
               (hash-map (target->str h)
                         v)))
        (apply merge))
   {}))

;; (-> empty-inventory
;;     (add-target ["username" "hostname" 22] 
;;               {"var1" "val1" 
;;                "var2" "val2"})
;;     (add-target ["username2" [192 168 100 69] 22]) ;; no vars
;;     (add-target ["username" "hostname2" 22] 
;;               {"var1" "vala" 
;;                "var2" "valb" 
;;                "var3" true})
;;     inv->ansible-hostvars)
;; {"hostname" {"var1" "val1", "var2" "val2"}, "hostname2" {"var1" "vala", "var2" "valb", "var3" true}, "ip4_192_168_100_69" {}}

;; ----------------------------------------

(s/fdef make-ansible-list
  :args (s/cat :inv ::inventory)
  :ret map?)

(defn make-ansible-list
  "Returns data appropriate for the ansible dynamic inventory script."
  [inv]
  (merge (inv->ansible--list-groups inv)
         {"_meta"
          {"hostvars" (inv->ansible-hostvars inv)}}))

;; ----------------------------------------

