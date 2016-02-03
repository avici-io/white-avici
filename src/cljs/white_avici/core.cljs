(ns white-avici.core
  (:require-macros [cljs.core.async.macros :refer [go]])
    (:require [reagent.core :as reagent :refer [atom]]
              [reagent.session :as session]
              [reagent.validation :as validation]
              [secretary.core :as secretary :include-macros true]
              [ajax.core :refer [GET POST]]
              [cljs-http.client :as http]
              [cljs.core.async :refer [<! chan]]
              [accountant.core :as accountant]))

(defonce state
  (atom
   {
    :games
    [
     {
      :title "abcdef"
      :id 1
      }
     {
      :title "123456"
      :id 2
      }
     ]
    :credentials {}
    :last-error nil}))

;; -------------------------
;; Views

(defn home-page []
  [:div [:h2 "Welcome to white-avici"]
   [:div [:a {:href "/about"} "go to about page"]]])

(defn logged-in? []
  (not (empty? (:credentials @state))))

(declare logout!)

(defn navbar-partial []
  [:div {:class "navbar"}
   [:ul
    [:li [:a {:href "/"} "Index"]]
    (if (logged-in?)
      [:div
       [:li [:a {:href "/me"} (-> @state :credentials :username)]]
       [:li>a {:on-click #(logout!)} "Logout"]]
      [:div
       [:li [:a {:href "/login"} "Login"]]
       [:li [:a {:href "/register"} "Register"]]])]])

(defn index-page []
  [:div
   (navbar-partial)
   [:input {:type "text"}]
   [:button "Search"]
   (let [games (:games @state)]
     (for [g games]
       [:a {:href (str "/games/" (:id g))} (:title g)]))])

(defn input-element
  [id name type value]
  [:input {:id id
           :name name
           :class "form-control"
           :type type
           :required ""
           :value @value
           :on-change #(reset! value (-> % .-target .-value))}])

(defn username-input
  [a]
  (input-element "username" "username" "text" a))

(defn password-input
  [a]
  (input-element "password" "password" "password" a))

(defn email-input
  [a]
  (input-element "email" "email" "email" a))

(defn register! [username password email status-atom]
  (go (let [response (<! (http/post "http://localhost:3022/user/new"
                                    {:form-params {:username username
                                                   :password password
                                                   :email email}}))]
        (case (:status response)
          201 (reset! status-atom (str "Registered as " username " !"))
          400 (reset! status-atom "Username already exists")
          500 (reset! status-atom "500 Server Error")))))

(defn login! [username password status-atom]
  (go (let [response (<! (http/post "http://localhost:3022/auth"
                                    {:form-params {:username username
                                                   :password password}}))]
        (case (:status response)
          200 (swap! state assoc-in [:credentials] {:username username
                                                    :key (:key (:body response))})
          403 (reset! status-atom "User found, password does not match")
          404 (reset! status-atom "User not found")))))

(defn logout! []
  (swap! state assoc-in [:credentials] {}))

(defn login-page []
  (let [username (atom nil)
        password (atom nil)
        status (atom nil)]
    (fn []
      (let [username-valid (and
                            (validation/max-length? @username 40)
                            (validation/min-length? @username 2))
            password-valid (and
                            (validation/max-length? @password 100)
                            (validation/min-length? @password 3))]
        [:div {:class "login-wrapper"}
         (navbar-partial)
         [:h2 "Login into Avici"]
         [:form {:id "form"}
          [username-input username]
          [password-input password]]
         [:button
          {:type "submit"
           :disabled (not (and username-input password-valid))
           :on-click #(login! @username @password status)}
          "Login"]
         [:p @status]]))))

(defn get-user-data! [target-atom status-atom]
  (go (let [res (<! (http/post "http://localhost:3022/auth/me"
                               {:form-params {:key (-> @state :credentials :key)}}))]
        (case (:status res)
          200 (reset! target-atom (:body res))
          400 (reset! status-atom 400)
          404 (reset! status-atom 404)))))

(defn get-game-data! [id target-atom status-atom]
  (go (let [res (<! (http/get (str "http://localhost:3022/game/" id)))]
        (case (:status res)
          200 (reset! target-atom (:body res))
          400 (reset! status-atom 400)
          404 (reset! status-atom 404)))))

(defn create-new-game! [target-atom status-atom]
  (go (let [key (:key (:credentials @state))
            res (<! (http/post "http://localhost:3022/game/new"
                               {:form-params {:key key
                                              :title "Untitled Game"
                                              :tagline "A short description to your game"
                                              :description "Description is fun"
                                              :category 0
                                              :public false}}))]
        (case (:status res)
          201 (do
                (reset! status-atom 200)
                (get-user-data! target-atom status-atom))
          400 (reset! status-atom 400)
          401 (reset! status-atom 401)
          403 (reset! status-atom 403)
          (println (:status res))))))

(defn patch-game! [game-id {:keys [title tagline description] :as data} status-atom]
  (go (let [key (:key (:credentials @state))
            res (<! (http/patch (str "http://localhost:3022/game/" game-id)
                                {:form-params (merge data {:key key})}))]
        (reset! status-atom (:status res)))))

(defn delete-game! [game-id target-atom status-atom]
  (go (let [key (:key (:credentials @state))
            res (<! (http/delete (str "http://localhost:3022/game/" game-id)
                                 {:form-params {:key key}}))]
        (case (:status res)
          200 (do
                (reset! status-atom 200)
                (get-user-data! target-atom status-atom))
          (reset! status-atom (:status res))))))

(defn me-games-edit-page [game-id]
  (let [user-data (atom nil)
        game-data (atom nil)
        load-status (atom nil)
        title (atom nil)
        description (atom nil)
        tagline (atom nil)
        category (atom nil)
        edit-status (atom nil)]
    (do
      (get-user-data! user-data load-status)
      (get-game-data! game-id game-data load-status)
      (fn []
        (do
          (println [@user-data (nil? @title)])
          (if (and @user-data @game-data (nil? @title))
            (let [game-data @game-data]
              (do
                (reset! title (-> game-data :title))
                (reset! description (-> game-data :description))
                (reset! tagline (-> game-data :tagline))
                (reset! category (-> game-data :category))))
            (println [@user-data (nil? @title)])))
        (let []
          [:div {:class "me-games-edit-page"}
           (navbar-partial)
           [:h2 (str "Me/Games/Edit:" game-id)]
           (if (nil? @user-data)
             [:h4 "Loading User Data"]
             [:div
              [:h4 "User Data Loaded"]
              [:h5 (str "Game ID: " game-id)]
              [:ul
               [:li>p (str "title: " @title)]]
              (if @edit-status
                [:h5 (str "Return Status: " @edit-status)])
              [:form
               (input-element "title" "title" "text" title)
               (input-element "description" "description" "text" description)
               (input-element "tagline" "tagline" "text" tagline)]
              [:button {:on-click #(patch-game! game-id
                                                {:title @title
                                                 :description @description
                                                 :tagline @tagline}
                                                edit-status)} "Submit"]])])))))

(defn me-games-page []
  (let [user-data (atom nil)
        load-status (atom nil)
        game-data (atom nil)]
    (do
      (get-user-data! user-data load-status)
      (fn []
        (do
          (println user-data))
        (let []
          [:div {:class "me-games-wrapper"}
           (navbar-partial)
           [:h2 "Me/Games"]
           (if (nil? @user-data)
             [:h4 "Loading User Data"]
             [:div
              [:h4 "User Data Loaded"]
              [:h5 "User Games"]
              [:p (str "Total Length: " (count (:games @user-data)))]
              [:div {:class "games"}
               (for [g (:games @user-data)]
                 ^{:key g}
                 [:div
                  [:h3 (:title g)]
                  [:ul
                   [:li (str "id: " (:id g))]
                   [:li (str "tagline: " (:tagline g))]]
                  [:a {:href (str "/me/games/" (:id g))} "Edit"]
                  [:button {:on-click
                            (fn []
                              (delete-game! (:id g) user-data load-status))} "Delete"]])]
              [:div {:class "management"}
               [:button {:on-click (fn []
                                     (create-new-game! user-data load-status))} "New Game"]]])])))))

(defn me-page []
  (let [user-data (atom nil)
        load-status (atom nil)]
    (do
      (get-user-data! user-data load-status)
      (fn []
        (do
          (println user-data)
        (let []
          [:div {:class "me-wrapper"}
           (navbar-partial)
           [:h2 "Me"]
           (if (nil? @user-data)
             [:h4 "Loading User Data"]
             [:div
              [:h4 "User Data Loaded"]
              [:h5 (str "username: " (:username @user-data))]
              [:h5 (str "email: " (:email @user-data))]
              [:h5 (str "games-count: " (count (:games @user-data)))]
              [:ul
               [:li>a {:href "/me/edit"} "Edit Profile"]
               [:li>a {:href "/me/games"} "Manage Games"]]])]))))))

(defn edit-profile-page []
  (let [user-data (atom nil)
        load-status (atom nil)]))

(defn register-page []
  (let [username (atom nil)
        password (atom nil)
        email (atom nil)
        status (atom nil)]
    (fn []
      (let [username-valid (and
                            (validation/max-length? @username 40)
                            (validation/min-length? @username 2))
            password-valid (and
                            (validation/max-length? @password 100)
                            (validation/min-length? @password 3))
            email-valid (validation/is-email? @email)]
        [:div {:class "register-wrapper"}
         (navbar-partial)
         [:h2 "Register Avici"]
         [:form {:id "form"}
          [username-input username]
          [password-input password]
          [email-input email]]
         [:button {:type "submit"
                   :disabled (not (and username-valid password-valid email-valid))
                   :on-click #(register! @username @password @email status)}
          "Submit"]
         [:div
          [:p (if username-valid
                "Username Valid"
                "Username must be between 2 and 40")]
          [:p (if password-valid
                "Password Valid"
                "Password must be between 3 and 100")]
          [:p (if email-valid
                "Email correct"
                "Wrong Email")]]
         [:p @status]
         [:p @email]]))))

(defn about-page []
  [:div [:h2 "About white-avici"]
   [:div [:a {:href "/"} "go to the home page"]]])

(defn current-page []
  [:div [(session/get :current-page)]])

;; -------------------------
;; Routes

(secretary/defroute "/" []
  (session/put! :current-page #'index-page))

(secretary/defroute "/about" []
  (session/put! :current-page #'home-page))

(secretary/defroute "/me" []
  (session/put! :current-page #'me-page))

(secretary/defroute "/me/games" []
  (session/put! :current-page #'me-games-page))

(secretary/defroute "/me/games/:id" {:as params}
  (session/put! :current-page (me-games-edit-page (js/parseInt (:id params)))))

(secretary/defroute "/register" []
  (session/put! :current-page #'register-page))

(secretary/defroute "/login" []
  (session/put! :current-page #'login-page))

;; -------------------------
;; Initialize app

(defn mount-root []
  (reagent/render [current-page] (.getElementById js/document "app")))

(defn init! []
  (accountant/configure-navigation!)
  (accountant/dispatch-current!)
  (mount-root))
