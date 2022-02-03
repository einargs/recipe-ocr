<template>
  <page-skeleton>
    <template #breadcrumbs>
      <el-breadcrumb-item :to="{ name: 'recipe-list' }">
        My Recipes
      </el-breadcrumb-item>
      <el-breadcrumb-item>
        {{ recipe.name }}
      </el-breadcrumb-item>
    </template>
    <template #header-right>
      <el-button
        @click="goToEdit"
        type="primary">Edit</el-button>
    </template>
    <el-descriptions
      class="recipe-desc"
      :title="recipe.name"
      border>
      <el-descriptions-item label="Tags">
        {{tagString}}
      </el-descriptions-item>
    </el-descriptions>
    <div class="image-list">
      <el-row v-for="url in recipe.images" :key="url">
        <el-col
          :span="24"
          :md="{span: 18, offset: 3}"
          :lg="{span: 14, offset: 5}">
          <el-image
            fit="fill"
            :src="url" />
        </el-col>
      </el-row>
    </div>
    <el-collapse>
      <el-collapse-item title="Content">
        <p v-for="(text, i) in paragraphs" :key="i">{{text}}</p>
      </el-collapse-item>
    </el-collapse>
  </page-skeleton>
</template>

<script>
import PageSkeleton from "./PageSkeleton.vue"
import { getRecipe } from "../Api.js"

export default {
  name: 'RecipeView',
  components: {PageSkeleton},
  data() {
    return {
      recipe: {
        name: "",
        tags: [],
        images: [],
        body: "",
      },
    }
  },
  computed: {
    paragraphs() {
      return this.$data.recipe.body
        .split(/\n+/g)
        .filter(str => str != "")
    },
    tagString() {
      return this.$data.recipe.tags.join(", ")
    },
  },
  methods: {
    goToEdit() {
      this.$router.push({ name: "recipe-edit" })
    },
  },
  created() {
    getRecipe(this.$route.params.id)
      .then(recipe => {
        this.$data.recipe = recipe
      })
  },
}
</script>

<style>
.recipe-desc {
  margin-bottom: 24px;
}
</style>
